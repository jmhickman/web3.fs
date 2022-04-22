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
        //| EthMethod.GetFilterChanges -> "eth_getFilterChanges" //
        //| EthMethod.GetFilterLogs -> "eth_getFilterLogs" //
        //| EthMethod.GetLogs -> "eth_getLogs" //
        | EthMethod.GetStorageAt -> "eth_getStorageAt" //
        | EthMethod.GetTransactionCount -> "eth_getTransactionCount" //
        | EthMethod.GetTransactionByHash -> "eth_getTransactionByHash" //
        | EthMethod.GetTransactionByBlockHashAndIndex -> "eth_getTransactionByBlockHashAndIndex" //
        | EthMethod.GetTransactionByBlockNumberAndIndex -> "eth_getTransactionByBlockNumberAndIndex" //
        | EthMethod.GetTransactionReceipt -> "eth_getTransactionReceipt" //
        | EthMethod.GetUncleCountByBlockHash -> "eth_getUncleCountByBlockHash" //
        | EthMethod.GetUncleCountByBlockNumber -> "eth_getUncleCountByBlockNumber" //
        //| EthMethod.NewFilter -> "eth_newFilter" //
        //| EthMethod.NewBlockFilter -> "eth_newBlockFilter" //
        //| EthMethod.NewPendingTransactionFilter -> "eth_newPendingTransactionFilter" //
        | EthMethod.ProtocolVersion -> "eth_protocolVersion" //
        | EthMethod.Syncing -> "eth_syncing" //
        | EthMethod.SendTransaction -> "eth_sendTransaction" //
        | EthMethod.SendRawTransaction -> "eth_sendRawTransaction" //
        | EthMethod.Sign -> "eth_sign" //
        | EthMethod.SignTransaction -> "eth_signTransaction" //
        //| EthMethod.UninstallFilter -> "eth_uninstallFilter" //

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Parameter module
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


module RPCParamFunctions =
    
    open Common

    
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

    
[<AutoOpen>]    
module RPCFunctions =
    
    open Common
    open Logging


    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ethereum call functions using the RPC connection
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
    
    
    ///
    /// Creates an unvalidated record
    let private returnUnvalidatedRecord address txn maxfee priority data contract value =
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
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Call Function Helpers
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

    
    ///
    /// Checks that the signer is on the chain we're about to transact with. Emits WrongChainInSigner if not.
    ///
    /// Check that the chain we're trying to work with is actually selected in the signer
    let private checkForChain env chainId  =
        { method = EthMethod.ChainId
          paramList = [] |> EthGenericRPC
          blockHeight =  LATEST}
          |> env.connection
          |> decomposeRPCResult EthMethod.ChainId
          |> log Emit
          |> unwrapSimpleValue
          |> fun chain ->
                if not(chain = chainId ) then
                    WrongChainInSigner |> Error
                else () |> Ok 
    
    
    ///
    /// Unpacks constants from the ContractConstants record
    let private unpackConstants constants (pipe: Result<'a, Web3Error>) =
        pipe
        |> Result.bind (fun _ ->
            let txn, maxfee, priority, _ = constantsBind constants
            (txn, maxfee, priority) |> Ok )
    
    
    ///
    /// Injects de-Optioned contract arguments
    let private unwrapContractArguments (args: EVMDatatype list option) (pipe: Result<string * string * string, Web3Error>) =
        match args with
        | Some d -> pipe |> Result.bind(fun (a, b, c) -> (a, b, c, d) |> Ok)
        | None -> pipe |> Result.bind(fun (a, b, c) -> (a, b, c, []) |> Ok)
        
        
    let buildDeploymentCall env value contract (pipe: Result<string * string * string * EVMDatatype list, Web3Error>) =
        let (RawContractBytecode _rawBytecode) = contract.bytecode
        pipe
        |> Result.bind (fun (txn, maxfee, priority, args) ->
            { utxnType = txn
              unonce = ""
              utoAddr = ""
              ufrom = env.constants.walletAddress
              ugas = "0x4C4B40"
              uvalue = value |> bigintToHex |> prepend0x
              udata =
                  $"{_rawBytecode}{createInputByteString args}" |> prepend0x              
              umaxFeePerGas = maxfee
              umaxPriorityFeePerGas = priority
              uaccessList = []
              uchainId = contract.chainId } |> Ok )
        
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Call Functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////     
    
        
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
    let public makeEthRPCCall env method (paramList: string list) =
        { method = method
          paramList = paramList |> EthGenericRPC
          blockHeight = LATEST }
        |> env.connection
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
    let public makeEthTxn env contract evmFunction arguments value =
        let blockHeight' = blockHeight env.constants
        
        let args =
            match arguments with
            | [] -> None
            | x -> Some x
        
        createUnvalidatedTxn env.constants contract evmFunction args value
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.SendTransaction 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> env.connection)
        |> Result.bind (fun r ->
            unpackRoot r
            |> stringAndTrim
            |> EthTransactionHash
            |> Ok)
        |> monitorTransaction env.monitor


    ///
    /// Creates an Ethereum call that does NOT change the state of the blockchain.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionIndicator corresponding to the the function being called. May be a string (cast to a
    /// ByString) or a IndicatedFunction (returned by `findFunction`)
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no arguments. 
    ///
    let public makeEthCall env contract evmFunction arguments =
        let blockHeight' = blockHeight env.constants

        let args =
            match arguments with
            | [] -> None
            | x -> Some x
            
        createUnvalidatedCall env.constants contract evmFunction args
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.Call 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> env.connection)
        |> Result.bind (fun r ->
            returnOutputAsEVMDatatypes contract evmFunction (unpackRoot r |> stringAndTrim)
            |> CallResult
            |> Ok)
    
    
    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain) specifically for deploying
    /// a contract's bytecode.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A UndeployedContract that is being deployed
    ///
    let public deployEthContract env value (contract: UndeployedContract) =
        checkForChain env contract.chainId
        |> unpackConstants env.constants
        |> unwrapContractArguments contract.constructorArguments
        |> buildDeploymentCall env value contract
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.SendTransaction 
                  paramList = _params 
                  blockHeight = LATEST }
                |> env.connection)
        |> Result.bind (fun r ->
            unpackRoot r
            |> stringAndTrim
            |> EthTransactionHash
            |> Ok)
        |> monitorTransaction env.monitor
        
        
    ///
    /// Estimate the amount of gas units required for the given transaction to complete. This number can be rather
    /// inaccurate, so the function allows the specification of additional padding in gas units.
    let public estimateGas units env contract evmFunction arguments value  =
        let blockHeight' = blockHeight env.constants
            
        let args =
            match arguments with
            | [] -> None
            | x -> Some x
        
        createUnvalidatedTxn env.constants contract evmFunction args value
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.EstimateGas 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> env.connection)
        |> fun res ->
            match res with
            | Ok o ->
                let gas = unpackRoot o |> stringAndTrim
                match units with
                | HexGas -> gas
                | DecimalGas -> gas |> strip0x |> hexToBigIntP |> fun i -> i.ToString()
            | Error e -> $"estimate gas call failed: {e}"
        