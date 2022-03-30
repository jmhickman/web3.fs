namespace web3.fs

open web3.fs.Types

module RPCConnector =
    open FsHttp
    open FsHttp.DslCE
    
     
    open Helpers
    open RPCMethodFunctions
    open RPCParamFunctions
    open RPCBindFunctions
    open ContractFunctions
    open ABIFunctions


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC helpers
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    

    ///
    /// Returns a Txn object for use in the validation function `ValidateRPCParams`
    let createUnvalidatedTxn
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        (value: string)
        : UnvalidatedEthParam1559Call =

        let txn, maxfee, priority, data = constantsBind constants

        let evmF =
            bindFunctionIndicator contract evmFunction

        let udata =
            match arguments with
            | Some a ->
                match checkEVMData a with
                | Ok _ -> $"{evmF.hash |> bindEVMSelector}{createInputByteString a}"
                | Error e -> $"{e.ToString}"
            | None ->
                match checkEVMData data with
                | Ok _ -> $"{evmF.hash |> bindEVMSelector}{createInputByteString data}"
                | Error e -> $"{e.ToString}"
        
        { utxnType = txn
          unonce = ""
          utoAddr = contract.address
          ufrom = constants.address
          ugas = ""
          uvalue = value |> bigintToHex |> padTo32BytesLeft
          udata = udata
          umaxFeePerGas = maxfee
          umaxPriorityFeePerGas = priority
          uaccessList = []
          uchainId = contract.chainId }


    ///
    /// Alias for `createUnvalidatedTxn` with the value set to 0.
    let createUnvalidatedCall
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        : UnvalidatedEthParam1559Call =

        createUnvalidatedTxn constants contract evmFunction arguments "0"


    ///
    /// Some RPC calls require a parameter indicating at what block height the call should be performed against. This
    /// finds calls that require it in order to insert a parameter passed in from the user, and ignores it otherwise.
    /// Note that many calls that come in the form of lists (rather than formatted JSON) include the block height
    /// parameter already, and so come through this function as 'false' because the parameter doesn't need to be
    /// handled here. 
    ///
    let needsBlockArgs (m: RPCMethod) =
        match m with
        | EthMethod _m ->
            match _m with
            | _m when _m = EthMethod.Call || _m = EthMethod.EstimateGas -> true
            | _ -> false
        | _ -> false


    ///
    /// Returns properly formatted JSON-RPC values to send to the RPC.
    let formatRPCString (rpcMsg: HttpRPCMessage) rpcVersion blockHeight blockArgs =

        match blockArgs with
        | true ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcMsg.method}","params":[{bindRPCParam rpcMsg.paramList}, "{blockHeight}"], "id":1}}"""
        | false ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcMsg.method}","params":[{bindRPCParam rpcMsg.paramList}], "id":1}}"""


    ///
    /// Returns a result based on the success or failure of the Http request.
    let requestHttpAsync url rjson = async {
        try
            let! response = httpAsync {
                POST url
                Origin "Web3.fs"
                ContentType "application/json"
                body
                json rjson
            }
            let! o = response.content.ReadAsStringAsync() |> Async.AwaitTask
            return o |> Ok
        with e -> return $"{e.Message}" |> HttpClientError |> Error
    }

    
    ///
    /// Handles certain types of responses from RPCs that come back with a `null` in the Result field, which causes
    /// issues if not handled first. This means the same response is essentially double-filtered, which is inefficient
    /// but not the slowest link in the chain.
    ///  
    let filterNullOrErrorResponse (s: string) =
        
        match RPCResponse.Parse(s) with
        | x when x.Result.IsSome  -> x |> Ok
        | x when x.Error.IsSome -> $"RPC error message: {x.Error.Value}" |> RPCResponseError |> Error
        | _ ->  RPCNullResponse |> Error
         
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC connection functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// A MailboxProcessor that manages making and returning RPC calls via a Reply channel.
    let rpcConnector (url: string) (rpcVersion: string) (mbox: HttpRPCMailbox) =
        
        let rec receiveLoop () =
            async {
                let! msg = mbox.Receive()
                let (TransactionMessageAndReply (rpcMessage, reply)) = msg
                
                rpcMessage.method
                |> needsBlockArgs
                |> formatRPCString rpcMessage rpcVersion rpcMessage.blockHeight
                //|> fun p -> printfn $"{p}"; p
                |> requestHttpAsync url
                |> Async.RunSynchronously                
                |> Result.bind filterNullOrErrorResponse
                |> reply.Reply
                
                do! receiveLoop ()
            }
        receiveLoop ()


    ///
    /// Returns the MailboxProcessor that oversees Http communications with the RPC
    let startRpcConnector (url: string) rpcVersion =
        MailboxProcessor.Start(rpcConnector url rpcVersion)


    ///
    /// Function allowing easier pipelining of RPC connection and the two-way communication channel.
    let transactionMessage (mbox: HttpRPCMailbox) (rpcMessage: HttpRPCMessage) =
        mbox.PostAndReply(fun c -> TransactionMessageAndReply(rpcMessage, c))


    ///
    /// Returns a partially applied function ready to take an HttpRPCMessage and send it to the RPC endpoint and
    /// return a Result.
    ///
    let createWeb3Connection url rpcVersion =
        (url, rpcVersion)
        ||> startRpcConnector
        |> transactionMessage


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ethereum call functions using the RPC connection
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Factored out for reuse. Passes through a specified blockheight, or supplies the LATEST default.
    let blockHeight (constants: ContractConstants) =
        match constants.blockHeight with
        | Some s -> s
        | None -> LATEST
    
    
    ///
    /// Creates an Ethereum RPC request whose purpose is typically to query the RPC node for chain-based or network-
    /// based data. Examples are retrieving the contents of a block, checking a transaction receipt, or getting an
    /// account balance.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * method: An EthMethod selector, like `EthMethod.GetBalance`
    /// * paramList: An EthParam, such as `["0x3872353821064f55df53ad1e2d7255e969f6eac0", "0x9dc3fe"]`
    /// Note that some EthMethods have no arguments, while others have object arguments. Use `` //TODO Use what?
    /// in the case of object arguments.
    ///
    let makeEthRPCCall
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>)
        (method: EthMethod)
        (paramList: EthParam)
        =

        rpcConnection
            { method = method |> wrapEthMethod
              paramList = paramList |> wrapEthParams
              blockHeight = LATEST }


    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain).
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionIndicator corresponding to the the function being called. May be a string (cast to a
    /// ByString) or a IndicatedFunction (returned by `findFunction`)
    /// * arguments: a list option of EVMDatatypes. May use `None` or `(Some [])` to indicate no arguments.
    /// value: the wei-denominated amount of ETH to send along with a transaction to a `payable` function.
    ///
    let makeEthTxn
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>)
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        (value: string)
        =

        let blockHeight' = blockHeight constants

        createUnvalidatedTxn constants contract evmFunction arguments value
        |> validateRPCParams
        |> Result.bind
            (fun _params ->
                rpcConnection
                    { method = EthMethod.SendTransaction |> wrapEthMethod
                      paramList = _params |> EthParam
                      blockHeight = blockHeight' })


    ///
    /// Creates an Ethereum call that does NOT change the state of the blockchain.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionIndicator corresponding to the the function being called. May be a string (cast to a
    /// ByString) or a IndicatedFunction (returned by `findFunction`)
    /// * arguments: a list option of EVMDatatypes. May use `None` or `(Some [])` to indicate no arguments.
    ///
    let makeEthCall
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>)
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        =

        let blockHeight' = blockHeight constants

        createUnvalidatedCall constants contract evmFunction arguments
        //|> fun p -> printfn $"{p}"; p
        |> validateRPCParams
        |> Result.bind
            (fun _params ->
                rpcConnection
                    { method = EthMethod.Call |> wrapEthMethod
                      paramList = _params |> EthParam
                      blockHeight = blockHeight' })


    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain) specifically for deploying
    /// a contract's bytecode.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A UndeployedContract that is being deployed
    ///
    let deployEthContract
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>)
        (constants: ContractConstants)
        (contract: UndeployedContract)
        =

        let (RawContractBytecode _rawBytecode) = contract.bytecode
        let txn, maxfee, priority, _ = constantsBind constants

        let cArgs =
            match contract.constructorArguments with
            | Some a -> a
            | None -> []
        
        { utxnType = txn
          unonce = ""
          utoAddr = ""
          ufrom = constants.address
          ugas = ""
          uvalue = "0" |> bigintToHex |> padTo32BytesLeft
          udata =
              $"{_rawBytecode}{contract.constructor |> bindEVMSelector |> strip0x}{createInputByteString cArgs}"
              |> prepend0x
          umaxFeePerGas = maxfee
          umaxPriorityFeePerGas = priority
          uaccessList = []
          uchainId = contract.chainId }
        |> validateRPCParams
        |> Result.bind
            (fun _params ->
                rpcConnection
                    { method = EthMethod.SendTransaction |> wrapEthMethod
                      paramList = _params |> EthParam
                      blockHeight = LATEST })

    
    
    ///
    /// Returns a MinedTransaction record based on a given transaction hash that has been included in a blockchain.
    /// Intended to be piped directly from a `bindTransactionResult`, but can be called alone.
    /// 
    let followupCallTransaction
        (web3c: HttpRPCMessage -> Result<RPCResponse.Root,Web3Error>)
        (r: Result<CallResponses,Web3Error>) =
            
        r |> Result.bind (fun r' ->
            match r' with
            | TransactionReceiptResult t ->
                makeEthRPCCall web3c EthMethod.GetTransactionByHash ([$"{t.transactionHash}"]|> EthParamGetTransactionByHash)
                |> bindGetTransactionByHashResult
            | x -> x |> Ok )
                
  