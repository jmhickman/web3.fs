namespace web3.fs

open web3.fs.Types

[<AutoOpen>]
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
    let private createUnvalidatedTxn constants contract evmFunction arguments value =
        let txn, maxfee, priority, data = constantsBind constants
        let iValue =
            value
            |> bigintToHex
            |> fun s -> s.TrimStart('0')// bigintToHex will stick extra 0s on that we don't need.
            |> prepend0x
        
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
        
        match evmF.config with
        | Payable -> 
            if value = "0" then logResult "WARN: 0 value being sent to payable function"
            { utxnType = txn
              unonce = ""
              utoAddr = contract.address
              ufrom = constants.walletAddress
              ugas = ""
              uvalue = iValue
              udata = udata
              umaxFeePerGas = maxfee
              umaxPriorityFeePerGas = priority
              uaccessList = []
              uchainId = contract.chainId }
            |> Ok
        | _ ->
            if not(value = "0") then
                ValueToNonPayableFunctionError |> Error
            else
                { utxnType = txn
                  unonce = ""
                  utoAddr = contract.address
                  ufrom = constants.walletAddress
                  ugas = ""
                  uvalue = iValue
                  udata = udata
                  umaxFeePerGas = maxfee
                  umaxPriorityFeePerGas = priority
                  uaccessList = []
                  uchainId = contract.chainId }
                |> Ok

    ///
    /// Alias for `createUnvalidatedTxn` with the value set to 0.
    let private createUnvalidatedCall constants contract evmFunction arguments =
        createUnvalidatedTxn constants contract evmFunction arguments "0"


    ///
    /// Some RPC calls require a parameter indicating at what block height the call should be performed against. This
    /// finds calls that require it in order to insert a parameter passed in from the user, and ignores it otherwise.
    /// Note that many calls that come in the form of lists (rather than formatted JSON) include the block height
    /// parameter already, and so come through this function as 'false' because the parameter doesn't need to be
    /// handled here. 
    ///
    let private needsBlockArgs (m: EthMethod) =
        match m with
        | _m when _m = EthMethod.Call || _m = EthMethod.EstimateGas -> true
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
            let! response = httpAsync {
                POST url
                Origin "Web3.fs"
                ContentType "application/json"
                body
                json rjson
                config_timeoutInSeconds 18.5
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
    let private filterNullOrErrorResponse (s: string) =
        match RPCResponse.Parse(s) with
        | x when x.Result.IsSome  -> x |> Ok
        | x when x.Error.IsSome -> $"RPC error message: {x.Error.Value}" |> RPCResponseError |> Error
        | _ ->  RPCNullResponse |> Error
         
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC connection functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// A MailboxProcessor that manages making and returning RPC calls via a Reply channel.
    let internal rpcConnector (url: string) (rpcVersion: string) (mbox: HttpRPCMailbox) =
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
    let private startRpcConnector (url: string) rpcVersion =
        MailboxProcessor.Start(rpcConnector url rpcVersion)


    ///
    /// Function allowing easier pipelining of RPC connection and the two-way communication channel.
    let private transactionMessage (mbox: HttpRPCMailbox) (rpcMessage: HttpRPCMessage) =
        mbox.PostAndReply(fun c -> TransactionMessageAndReply(rpcMessage, c))


    ///
    /// Returns a partially applied function ready to take an HttpRPCMessage and send it to the RPC endpoint and
    /// return a Result.
    ///
    let public createWeb3Connection url rpcVersion : Web3Connection=
        (url, rpcVersion)
        ||> startRpcConnector
        |> transactionMessage


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ethereum call functions using the RPC connection
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Factored out for reuse. Passes through a specified blockheight, or supplies the LATEST default.
    let private blockHeight (constants: ContractConstants) =
        match constants.blockHeight with
        | Some s -> s
        | None -> LATEST
    
    
    ///
    /// Returns the current block, meaning the last block at was included in the chain.
    let private returnCurrentBlock (result: RPCResponse.Result) =
        result |> stringAndTrim
    
    ///
    /// Returns a record of a mined transaction for use in decomposeResult 
    let private returnMinedTransactionRecord (result: RPCResponse.Result) =
        let mined = RPCMinedTransaction.Parse(result.JsonValue.ToString()) 
        let access = mined.AccessList |> Array.fold(fun acc i -> $"{acc}{i.ToString()}" ) ""
        
        { MinedTransaction.accessList = [access]
          MinedTransaction.blockHash = mined.BlockHash
          MinedTransaction.blockNumber = mined.BlockNumber
          MinedTransaction.chainId = mined.ChainId
          MinedTransaction.from = mined.From |> EthAddress
          MinedTransaction.gas = mined.Gas
          MinedTransaction.gasPrice = mined.GasPrice
          MinedTransaction.hash = mined.Hash |> EthTransactionHash
          MinedTransaction.input = mined.Input
          MinedTransaction.maxFeePerGas = mined.MaxFeePerGas
          MinedTransaction.maxPriorityFeePerGas = mined.MaxPriorityFeePerGas
          MinedTransaction.nonce = mined.Nonce
          MinedTransaction.r = mined.R
          MinedTransaction.s = mined.S
          MinedTransaction.v = mined.V
          MinedTransaction.toAddr = mined.To |> EthAddress
          MinedTransaction.transactionIndex = mined.TransactionIndex
          MinedTransaction.tType = mined.Type
          MinedTransaction.value = mined.Value }
        
    
    ///
    /// Returns a record of an Ethereum block for use in `decomposeResult` 
    ///
    let returnEthBlock (result: RPCResponse.Result) =
        let ethBlock = RPCBlock.Parse(result.JsonValue.ToString())
        let uncles = ethBlock.Uncles |> Array.fold(fun acc i -> $"{acc}{i.ToString()}" ) ""
        { author = ethBlock.Author
          baseFeePerGas = ethBlock.BaseFeePerGas
          difficulty = ethBlock.Difficulty
          extraData = ethBlock.ExtraData
          gasLimit = ethBlock.GasLimit
          gasUsed = ethBlock.GasUsed
          hash = ethBlock.Hash
          logsBloom = ethBlock.LogsBloom
          miner = ethBlock.Miner
          number = ethBlock.Number
          parentHash = ethBlock.ParentHash
          receiptsRoot = ethBlock.ReceiptsRoot
          sealFields = ethBlock.SealFields |> Array.toList
          sha3Uncles = ethBlock.Sha3Uncles
          size = ethBlock.Size
          stateRoot = ethBlock.StateRoot
          timestamp = ethBlock.Timestamp
          totalDifficulty = ethBlock.TotalDifficulty
          transactions = ethBlock.Transactions |> Array.toList
          transactionsRoot = ethBlock.TransactionsRoot
          uncles = [uncles] }
    
    /// Returns a decomposed RPC response record matching the output of the given EthMethod
    let private decomposeResult (method: EthMethod) (r: Result<RPCResponse.Root, Web3Error>) : CallResponses =
        r
        |> Result.bind (
            fun root ->
                let result = unpackRoot root
                match method with
                | EthMethod.GetTransactionByHash -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionByBlockHashAndIndex -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionByBlockNumberAndIndex -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.BlockNumber -> returnCurrentBlock result |> CurrentBlock |> Ok
                | EthMethod.GetBlockByNumber -> returnEthBlock result |> Block |> Ok
                | _ -> Null |> Ok
            )
        |> fun m ->
            match m with
            | Ok o -> o
            | Error _ -> Null
        
    ///
    /// Creates an Ethereum RPC request whose purpose is typically to query the RPC node for chain-based or network-
    /// based data. Examples are retrieving the contents of a block, checking a transaction receipt, or getting an
    /// account balance.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * method: An EthMethod selector, like `EthMethod.GetBalance`
    /// * paramList: An EthParam, such as `["0x3872353821064f55df53ad1e2d7255e969f6eac0", "0x9dc3fe"]`
    /// Note that some EthMethods have no arguments, while others have object arguments. Use `makeEthCall` or
    /// `makeEthTxn` in those cases.
    ///
    let public makeEthRPCCall (rpcConnection: Web3Connection) method (paramList: string list) =
        { method = method
          paramList = paramList |> EthGenericRPC
          blockHeight = LATEST }
        |> rpcConnection
        |> logRPCResult
        |> decomposeResult method
        


    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain).
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionIndicator corresponding to the the function being called. May be a string (cast to a
    /// ByString) or a IndicatedFunction (returned by `findFunction`)
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no arguments. 
    /// value: the wei-denominated amount of ETH to send along with a transaction to a `payable` function.
    ///
    let public makeEthTxn (rpcConnection: Web3Connection) constants contract evmFunction arguments value =
        let blockHeight' = blockHeight constants
        
        let args =
            match arguments with
            | [] -> None
            | x -> Some x
        
        createUnvalidatedTxn constants contract evmFunction args value
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.SendTransaction 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> rpcConnection)
        |> Result.bind (fun r -> unpackRoot r |> stringAndTrim |> EthTransactionHash |> Ok)            


    ///
    /// Creates an Ethereum call that does NOT change the state of the blockchain.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionIndicator corresponding to the the function being called. May be a string (cast to a
    /// ByString) or a IndicatedFunction (returned by `findFunction`)
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no arguments. 
    ///
    let public makeEthCall (rpcConnection: Web3Connection) constants contract evmFunction arguments =
        let blockHeight' = blockHeight constants

        let args =
            match arguments with
            | [] -> None
            | x -> Some x
            
        createUnvalidatedCall constants contract evmFunction args
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.Call 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> rpcConnection)
        |> Result.bind (fun r ->
            let unpacked = unpackRoot r |> stringAndTrim
            {raw = unpacked
             typed = returnOutputAsEVMDatatypes contract evmFunction unpacked}
            |> CallResult
            |> Ok)
    
    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain) specifically for deploying
    /// a contract's bytecode.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A UndeployedContract that is being deployed
    ///
    let public deployEthContract (rpcConnection: Web3Connection) constants contract =
        let (RawContractBytecode _rawBytecode) = contract.bytecode
        let txn, maxfee, priority, _ = constantsBind constants

        let cArgs =
            match contract.constructorArguments with
            | Some a -> a
            | None -> []
        
        { utxnType = txn
          unonce = ""
          utoAddr = ""
          ufrom = constants.walletAddress
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
                { method = EthMethod.SendTransaction 
                  paramList = _params 
                  blockHeight = LATEST }
                |> rpcConnection)
        |> logRPCResult
        |> Result.bind (fun r -> unpackRoot r |> stringAndTrim |> EthTransactionHash |> Ok)
