namespace web3.fs

open web3.fs.Types

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RPC method module
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


module RPCMethodFunctions =
    
  
    ///
    /// Binds EthMethod to a string representation of the desired call. Only making effort to support methods outlined
    /// at
    /// https://playground.open-rpc.org/?schemaUrl=https://raw.githubusercontent.com/ethereum/eth1.0-apis/assembled-spec/
    /// 
    let internal bindEthMethod (m: EthMethod) =
        match m with
        | EthMethod.Accounts -> "eth_accounts" //
        | EthMethod.BlockNumber -> "eth_blockNumber" //
        | EthMethod.Call -> "eth_call" //
        | EthMethod.Coinbase -> "eth_coinbase" //
        | EthMethod.ChainId -> "eth_chainId" //
        | EthMethod.EstimateGas -> "eth_estimateGas" //
        //| EthMethod.FeeHistory -> "eth_feeHistory" //
        | EthMethod.GasPrice -> "eth_gasPrice" //
        | EthMethod.GetBalance -> "eth_getBalance" //
        | EthMethod.GetBlockByHash -> "eth_getBlockByHash" //
        | EthMethod.GetBlockByNumber -> "eth_getBlockByNumber" //
        | EthMethod.GetBlockTransactionCountByHash -> "eth_getBlockTransactionCountByHash" //
        | EthMethod.GetBlockTransactionCountByNumber -> "eth_getBlockTransactionCountByNumber" //
        | EthMethod.GetCode -> "eth_getCode" //
        | EthMethod.GetFilterChanges -> "eth_getFilterChanges" //
        | EthMethod.GetFilterLogs -> "eth_getFilterLogs" //
        | EthMethod.GetLogs -> "eth_getLogs" //
        | EthMethod.GetStorageAt -> "eth_getStorageAt" //
        | EthMethod.GetTransactionCount -> "eth_getTransactionCount" //
        | EthMethod.GetTransactionByHash -> "eth_getTransactionByHash" //
        | EthMethod.GetTransactionByBlockHashAndIndex -> "eth_getTransactionByBlockHashAndIndex" //
        | EthMethod.GetTransactionByBlockNumberAndIndex -> "eth_getTransactionByBlockNumberAndIndex" //
        | EthMethod.GetTransactionReceipt -> "eth_getTransactionReceipt" //
        | EthMethod.GetUncleCountByBlockHash -> "eth_getUncleCountByBlockHash" //
        | EthMethod.GetUncleCountByBlockNumber -> "eth_getUncleCountByBlockNumber" //
        | EthMethod.NewFilter -> "eth_newFilter" //
        | EthMethod.NewBlockFilter -> "eth_newBlockFilter" //
        | EthMethod.NewPendingTransactionFilter -> "eth_newPendingTransactionFilter" //
        | EthMethod.ProtocolVersion -> "eth_protocolVersion" //
        | EthMethod.Syncing -> "eth_syncing" //
        | EthMethod.SendTransaction -> "eth_sendTransaction" //
        | EthMethod.SendRawTransaction -> "eth_sendRawTransaction" //
        | EthMethod.Sign -> "eth_sign" //
        | EthMethod.SignTransaction -> "eth_signTransaction" //
        | EthMethod.UninstallFilter -> "eth_uninstallFilter" //

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Parameter module
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


module RPCParamFunctions =
    
    open Helpers

    
    //
    // Convert call params into json string representation. RPC commands that consume filters will not work, as there
    // is no websocket facility set up as of 0.2.0.
    //
    let internal bindEthParam (p: EthParam) =
        match p with
        | EthGenericRPC p -> concatParamString p
        | EthParam1559Call _e -> createJsonObj _e
        | EthParam1559EstimateGas _e -> createJsonObj _e
        | EthParam1559SendTransaction _e -> createJsonObj _e
        | EthParam1559SignTransaction _e -> createJsonObj _e
        

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Bind Functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
[<AutoOpen>]    
module RPCFunctions =
    
    open FSharp.Data
    open web3.fs.Helpers
    
    ///
    /// Generic logger until I import something more featureful.
    let internal logResult logString =
        printfn $"{logString}"
        
           
    ///
    /// 
    let logWeb3Error (r: Result<'a, Web3Error>) =
        match r with
        | Ok o -> o |> Ok
        | Error e ->
            printfn $"Web3Error: {e}"
            e |> Error
    
    
    ///
    /// Emits call response to console. Will replace with proper logging in a future version.
    let public logRPCResult (r: Result<RPCResponse.Root,Web3Error>) =
        match r with
        | Ok o ->
            logResult $"RPC returned:\n{o.Result.Value}"
            o |> Ok
        | Error e ->
            logResult $"RPC Error:\n{e}"
            e |> Error
    
    
    ///
    /// 
    let public logCallResult indicator (r: Result<CallResponses, Web3Error>) =
        match r with
        | Ok o ->
            match o with
            | CallResult _r ->
                match indicator with
                | Typed ->
                    printfn $"{_r.typed}"
                    o |> Ok
                | Raw ->
                    printfn $"{_r.raw}"
                    o |> Ok
            | _ -> o |> Ok
        | Error e ->
            printfn $"Got Web3Error: {e}"
            e |> Error
    
    
    ///
    /// Binds and starts the transaction monitor if a transaction hash was emitted from `makeEthTxn`. Intended to be
    /// placed in a transaction pipeline to provide realtime logging of transaction completion.
    /// 
    let public monitorTransaction (monitor: Monitor) (r: Result<EthTransactionHash, Web3Error>) =
        
        match r with
        | Ok o ->
            logResult $"Beginning monitoring of transaction {o}"
            monitor o
        | Error _ -> Web3Error
        
    
    ///
    /// Unpacks Result from a RPCResponse.Root for logging
    let internal unpackRoot (r:RPCResponse.Root) =
        match r.Result with
        | Some r' -> r'
        | None -> RPCResponse.Result(JsonValue.Null)
    

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ethereum call functions using the RPC connection
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
    
    
    ///
    ///
    let returnUnvalidatedRecord address txn maxfee priority data contract value =
        { utxnType = txn
          unonce = ""
          utoAddr = contract.address
          ufrom = address
          ugas = ""
          uvalue = value
          udata = data
          umaxFeePerGas = maxfee
          umaxPriorityFeePerGas = priority
          uaccessList = []
          uchainId = contract.chainId }
        
    
    ///
    /// Returns the data string to be included in the transaction based on the presence of arguments in the original
    /// function call. Otherwise, the default arguments supplied in the ContractConstants will be used. Embeds error
    /// strings from the input validation layer into the data, which will be surfaced downstream of this function call.
    /// 
    let createArguments (evmFunction: EVMFunction) arguments data =
        match arguments with
            | Some a ->
                match checkEVMData a with
                | Ok _ -> $"{evmFunction.hash |> bindEVMSelector}{createInputByteString a}"
                | Error e -> $"{e.ToString}"
            | None ->
                match checkEVMData data with
                | Ok _ -> $"{evmFunction.hash |> bindEVMSelector}{createInputByteString data}"
                | Error e -> $"{e.ToString}"
    
    
    ///
    /// Returns a Txn object for use in the validation function `ValidateRPCParams`
    let private createUnvalidatedTxn constants contract evmFunction arguments value =
        let txn, maxfee, priority, data = constantsBind constants
        let hexValue = value |> bigintToHex |> prepend0x
        let evmFunction' = bindFunctionIndicator contract evmFunction
        let udata = createArguments evmFunction' arguments data            
        
        match evmFunction'.config with
        | Payable -> 
            if hexValue = "0x0" then logResult "WARNING: 0 value being sent to payable function"
            
            returnUnvalidatedRecord constants.walletAddress txn maxfee priority udata contract hexValue
            |> Ok
        | _ ->
            if not(hexValue = "0x0") then 
                ValueToNonPayableFunctionError |> Error
            else
                returnUnvalidatedRecord constants.walletAddress txn maxfee priority udata contract hexValue
                |> Ok

    
    ///
    /// Alias for `createUnvalidatedTxn` with the value set to 0.
    let private createUnvalidatedCall constants contract evmFunction arguments =
        createUnvalidatedTxn constants contract evmFunction arguments "0"

    
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
    /// Creates a TransactionReceiptResult from an incoming RPCResponse.
    let private returnTransactionReceiptRecord (result: RPCResponse.Result) =
        { blockHash = result.BlockHash
          blockNumber = result.BlockNumber
          contractAddress = result.ContractAddress.JsonValue.ToString() |> trimParameter |> Some
          cumulativeGasUsed = result.CumulativeGasUsed
          effectiveGasPrice = result.EffectiveGasPrice
          from = result.From
          gasUsed = result.GasUsed
          logs = result.Logs |> Array.map (fun l -> l.JsonValue.ToString()) |> Array.toList
          logsBloom = result.LogsBloom
          status = result.Status
          toAddr = result.To.JsonValue.ToString() |> EthAddress
          transactionHash = result.TransactionHash
          transactionIndex = result.TransactionIndex
          tType = result.Type }
           
    
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
    let private returnEthBlock (result: RPCResponse.Result) =
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
    
    
    ///
    /// Returns a decomposed RPC response record matching the output of the given EthMethod
    let internal decomposeRPCResult (method: EthMethod) (r: Result<RPCResponse.Root, Web3Error>) : CallResponses =
        r
        |> Result.bind (
            fun root ->
                let result = unpackRoot root
                match method with
                | EthMethod.BlockNumber -> returnCurrentBlock result |> CurrentBlock |> Ok
                | EthMethod.GetBlockByNumber -> returnEthBlock result |> Block |> Ok
                | EthMethod.GetTransactionByBlockHashAndIndex -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionByBlockNumberAndIndex -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionByHash -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionReceipt -> returnTransactionReceiptRecord result |> TransactionReceiptResult |> Ok
                | _ -> Web3Error |> Ok
            )
        |> fun m ->
            match m with
            | Ok o -> o
            | Error _ -> Web3Error
        
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
        |> decomposeRPCResult method
        


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
    let public deployEthContract (rpcConnection: Web3Connection) constants value contract  =
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
          ugas = "0x4C4B40"
          uvalue = value |> bigintToHex |> prepend0x
          udata =
              $"{_rawBytecode}{createInputByteString cArgs}" |> prepend0x              
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