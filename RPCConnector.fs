namespace web3.fs

open Types

[<AutoOpen>]
module RPCConnector =
    open FsHttp    
          
    open RPCMethodFunctions
    open RPCParamFunctions
    
    GlobalConfig.defaults
    |> Config.timeoutInSeconds 28.5
    |> GlobalConfig.set
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Helper Functions   
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Some RPC calls require a parameter indicating at what block height the call should be performed against. This
    /// finds calls that require it in order to insert a parameter passed in from the user, and ignores it otherwise.
    /// Note that many calls that come in the form of lists (rather than formatted JSON) include the block height
    /// parameter already, and so come through this function as 'false' because the parameter doesn't need to be
    /// handled here. 
    ///
    let private needsBlockArgs method =
        match method with
        | m when
            m = EthMethod.Call ||
            m = EthMethod.EstimateGas -> true
        | _ -> false
        

    ///
    /// Returns properly formatted JSON-RPC values to send to the RPC.
    let private formatRPCString (rpcMsg: HttpRPCMessage) ver blockHeight blockArgs =
        let method = bindEthMethod rpcMsg.method
        let par = bindEthParam rpcMsg.paramList
        
        match blockArgs with
        | true -> $"""{{"jsonrpc":"{ver}","method":"{method}","params":[{par}, "{blockHeight}"], "id":1}}"""
        | false -> $"""{{"jsonrpc":"{ver}","method":"{method}","params":[{par}], "id":1}}"""


    ///
    /// Returns a result based on the success or failure of the Http request.
    let private requestHttpAsync url rjson = async {
        try
            let! response =
                http { POST url
                       Origin "Web3.fs"
                       body
                       json rjson }
                |> Request.sendAsync
            let! o = response |> Response.toTextAsync
            return o |> Ok
        with e -> return $"{e.Message}" |> HttpClientError |> Error
    }

    
    ///
    /// Handles certain types of responses from RPCs that come back with a `null` in the Result field, which causes
    /// issues if not handled first. This means the same response is essentially double-filtered, which is inefficient
    /// but not the slowest link in the chain.
    ///  
    let private filterNullOrErrorResponse rpcResponse =
        match RPCResponse.Parse(rpcResponse) with
        | x when x.Result.IsSome  -> x |> Ok
        | x when x.Error.IsSome -> $"RPC error message: {x.Error.Value}" |> RPCResponseError |> Error
        | _ ->  RPCNullResponse |> Error
         
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC connection functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// A MailboxProcessor that manages making and returning RPC calls via a Reply channel.
    let internal rpcConnector url rpcVersion (mbox: HttpRPCMailbox) =
        let rec receiveLoop () =
            async {
                let! msg = mbox.Receive()
                let (TransactionMessageAndReply (rpcMessage, reply)) = msg
                
                rpcMessage.method
                |> needsBlockArgs
                |> formatRPCString rpcMessage rpcVersion rpcMessage.blockHeight
                |> requestHttpAsync url
                |> Async.RunSynchronously                
                |> Result.bind filterNullOrErrorResponse
                |> reply.Reply
                
                do! receiveLoop ()
            }
        receiveLoop ()


    ///
    /// Returns the MailboxProcessor that oversees Http communications with the RPC
    let private startRpcConnector url rpcVersion =
        MailboxProcessor.Start(rpcConnector url rpcVersion)


    ///
    /// Function allowing easier pipelining of RPC connection and the two-way communication channel.
    let private transactionMessage (mbox: HttpRPCMailbox) rpcMessage =
        mbox.PostAndReply(fun c -> TransactionMessageAndReply(rpcMessage, c))


    ///
    /// Returns a partially applied function ready to take an HttpRPCMessage and send it to the RPC endpoint and
    /// return a Result. Called automatically in `createWeb3Environment`
    ///
    let public createWeb3Connection url rpcVersion =
        (url, rpcVersion)
        ||> startRpcConnector
        |> transactionMessage



