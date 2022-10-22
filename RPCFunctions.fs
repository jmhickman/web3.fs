namespace Web3.fs

module RPCMethodFunctions =
    
  
    ///
    /// Binds EthMethod to a string representation of the desired call. Only
    /// making effort to support methods outlined at
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
    // Convert call params into json string representation. RPC commands that
    // consume filters will not work, as there is no websocket support.
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
    // Builder functions for creating unvalidated transaction objects
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
    
    
    ///
    /// Detects if an ENS name was used and performs the lookup to resolve it
    /// to an address.
    /// 
    let handleENSName env chainId (name: string) =
        if name.Contains('.') then
            let hash = convertENSName name
            let bytes = [Byte32 hash] |> createInputByteString |> function Ok o -> o | Error _ -> "" 
            let evmData = "02571be3" + bytes |> prepend0x
            { utxnType = "0x2"
              unonce = ""
              utoAddr = "0x00000000000C2E074eC69A0dFb2997BA6C7d2e1e"
              ufrom = env.signerAddress
              ugas = ""
              uvalue = ZEROHEX
              udata = evmData
              umaxPriorityFeePerGas = ""
              umaxFeePerGas = ""
              uaccessList = []
              uchainId = chainId}
            |> validateRPCParams
            |> Result.bind
                (fun _params ->
                    { method = EthMethod.Call 
                      paramList = _params 
                      blockHeight = LATEST }
                    |> env.connection)
           |> fun res ->
               match res with
               | Ok o -> o |> unpackRoot |> stringAndTrim |> fun s -> s.Remove(0, 26) |> prepend0x
               | Error _ -> "failed to resolve address"
        else name

    
    ///
    /// Convert the supplied value into hex form
    let private convertValueToHex value  =
        value |> bigintToHex
        
    
    ///
    /// Places the actual EVMFunction into the data for later use
    let private pipeBindFunction evmFunction (value: string) =
        value, (bindFunctionIndicator evmFunction)
        
    
    ///
    /// Checks that we aren't trying to call the fallback or receive function(s)
    /// on a contract that doesn't have them.
    /// 
    let checkForFallbackOrReceive contract (pipe: string * EVMFunction) =
        let _, requestedFunction = pipe
        if requestedFunction.name = "fallback" && contract.hasFallback = false then
            ContractLacksFallbackError |> Error
        else if requestedFunction.name = "receive" && contract.hasReceive = false then
            ContractLacksReceiveError |> Error
        else pipe |> Ok
    
    
    ///
    /// Checks that arguments aren't supplied to functions with an empty
    /// argument list, and that arguments aren't missing when required.
    /// 
    let private checkArgsToFunction (arguments: EVMDatatype list) (pipe: Result<string * EVMFunction, Web3Error>) =
        pipe |> Result.bind (fun pipe' ->
            let _, evmFunction = pipe'    
            match evmFunction.canonicalInputs with
            |x when bindEVMFunctionInputs x = "()" ->
                if arguments.Length = 0 then pipe' |> Ok
                else ArgumentsToEmptyFunctionSignatureError |> Error
            | _ ->                
                if not(arguments.Length = 0) then pipe' |> Ok
                else FunctionArgumentsMissingError |> Error )
    
    
    ///
    /// Check for sending value to a non-Payable.
    let private checkValueAndStateMutability (pipe: Result<string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind (fun pipe' ->
            let value, evmFunction = pipe'
            match evmFunction.config with
            | Payable -> pipe' |> Ok
            | _ ->
                if not(value = "0x0") then ValueToNonPayableFunctionError |> Error
                else pipe' |> Ok )
    
    
    ///
    /// Create the 'data' value string from arguments and function
    let private createDataString arguments (pipe: Result<string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind (fun (value, evmFunction) ->
            createInputByteString arguments
            |> Result.bind (fun calldata -> (value, $"{evmFunction.hash}{calldata}") |> Ok))
            
            
    ///
    /// Creates an unvalidated record 
    let private returnUnvalidatedRecord address contract (pipe: Result<string * string, Web3Error>)  =
        pipe
        |> Result.bind (fun (value, data) -> 
        { utxnType = EIP1159
          unonce = ""
          utoAddr = contract.address
          ufrom = address
          ugas = ""
          uvalue = value
          udata = data
          umaxFeePerGas = "" 
          umaxPriorityFeePerGas = "" 
          uaccessList = []
          uchainId = contract.env.chainId } |> Ok)
        
    
    ///
    /// Returns an unvalidated transaction object.
    let private createUnvalidatedTxn env contract (arguments: EVMDatatype list) value (pipe: Result<FunctionIndicator, Web3Error>) =
        pipe
        |> Result.bind (fun functionIndicator ->
            value
            |> convertValueToHex 
            |> pipeBindFunction functionIndicator
            |> checkForFallbackOrReceive contract 
            |> checkArgsToFunction arguments
            |> checkValueAndStateMutability
            |> createDataString arguments
            |> returnUnvalidatedRecord env.signerAddress contract)
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Deployment Function Helpers
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
    
    
    ///
    /// Checks that the value string isn't just empty "".
    let private checkForEmptyValueString (value: Wei) =
        if value.Length = 0 then () |> Ok else InvalidValueArgumentError |> Error
    
    
    ///
    /// Checks that the function we're attempting to call exists.
    let private checkFunctionExists contract functionSelector (pipe: Result<unit, Web3Error>) =
        pipe
        |> Result.bind (fun _ -> findFunction contract functionSelector)


    ///
    /// Check that we aren't supplying value to a non-Payable constructor, which
    /// will be accepted but the transaction will fail with a status 0x0.
    /// 
    let private checkValueAndStateMutabilityDeploy value contract (pipe: Result<unit, Web3Error>) =
        match contract.stateMutability with
        | Payable -> pipe |> Result.bind(fun pipe' -> pipe'|> Ok )
        | _ ->
            if value = "0" then
                pipe |> Result.bind(fun pipe' -> pipe' |> Ok )
            else ValueToNonPayableFunctionError |> Error
    
    
    ///
    /// Returns the actual bytestring of the contract for the transaction.
    let private createDeployData args (pipe: Result<unit,Web3Error>) =
        pipe
        |> Result.bind (fun _ ->
            match createInputByteString args with
            | Ok data -> data |> Ok
            | Error e -> e |> Error)
            
    
    ///
    /// Creates the unvalidated call record specifically for deployment.
    let private buildDeploymentCall value contract (pipe: Result<string, Web3Error>) =
        let (RawContractBytecode _rawBytecode) = contract.bytecode
        if contract.stateMutability = Payable && value = "0" then
            contract.env.log (PayableFunctionZeroValueWarning "Zero value sent to payable function" |> Error) |> ignore
        
        pipe
        |> Result.bind (fun data ->
            { utxnType = EIP1159
              unonce = ""
              utoAddr = ""
              ufrom = contract.env.signerAddress
              ugas = "0x2DC6C0" // 3 million gaslimit
              uvalue = value |> bigintToHex
              udata =
                  $"{_rawBytecode}{data}" |> prepend0x           
              umaxFeePerGas = ""
              umaxPriorityFeePerGas = ""
              uaccessList = []
              uchainId = contract.env.chainId } |> Ok )
        
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Call Functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////     
    
        
    ///
    /// Creates an Ethereum RPC request whose purpose is typically to query the
    /// RPC node for chain-based or network-based data. Examples are retrieving
    /// the contents of a block, checking a transaction receipt, or getting an
    /// account balance.
    /// * method: An EthMethod selector, like `EthMethod.GetBalance`
    /// * paramList: A string list, such as
    ///     `["0x3872353821064f55df53ad1e2d7255e969f6eac0", "0x9dc3fe"]`
    /// * chainId: The hexadecimal representation of the chain ID.
    /// * env: A Web3Environment. See createWeb3Environment.
    /// Some EthMethods have no arguments. Use an empty list in those cases.
    ///
    let public rpcCall method (paramList: string list) env =
        { method = method
          paramList = paramList |> EthGenericRPC
          blockHeight = LATEST }
        |> env.connection
        |> decomposeRPCResult method


    ///
    /// Creates a contract transaction (a call that changes the state of the
    /// blockchain).
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionSelector corresponding to the the function being
    ///     called. Typically (ByName "SomeFunction")
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no
    ///     arguments.
    /// * value: the wei-denominated amount of ETH to send along with a
    ///     transaction to a payable function.
    /// * env: An Web3Environment. See createWeb3Environment.
    ///
    let public contractTransaction evmFunction arguments value contract =
        
        checkForEmptyValueString value
        |> checkFunctionExists contract evmFunction 
        |> createUnvalidatedTxn contract.env contract arguments value
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.SendTransaction 
                  paramList = _params 
                  blockHeight = LATEST }
                |> contract.env.connection)
        |> Result.bind (fun r ->
            unpackRoot r
            |> stringAndTrim
            |> EthTransactionHash
            |> fun ethHash ->
                contract.env.log (Library $"Monitoring transaction {ethHash}\n" |> Ok )
                |> fun _ -> ethHash
            |> Ok)
        |> monitorTransaction contract.env.monitor
        
    
    
    ///
    /// Creates a contract call, a gasless transaction for querying the chain.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionSelector corresponding to the the function being
    ///     called. Typically '(ByName "SomeFunction")'
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no
    ///     arguments.
    /// * env: An Web3Environment. See createWeb3Environment.
    ///
    let public contractCall evmFunction arguments contract =
        let blockHeight' = LATEST
        
        () |> Ok// kind of stupid but whatever 
        |> checkFunctionExists contract evmFunction
        |> createUnvalidatedTxn contract.env contract arguments "0"  
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.Call 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> contract.env.connection)
        |> Result.bind (fun r ->
            returnOutputAsEVMDatatypes contract evmFunction (unpackRoot r |> stringAndTrim)
            |> CallResult
            |> Ok)
    
    
    ///
    /// Creates a transaction specifically for deploying a contract's bytecode.
    /// * contract: A UndeployedContract that is being deployed
    /// * value: the wei-denominated amount of ETH to send along with a
    ///     transaction to a payable constructor.
    /// * env: An Web3Environment. See createWeb3Environment.
    ///
    let public deployContract (value: Wei) (contract: UndeployedContract) =
        
        checkForEmptyValueString value
        |> checkValueAndStateMutabilityDeploy value contract
        |> createDeployData contract.constructorArguments 
        |> buildDeploymentCall value contract
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.SendTransaction 
                  paramList = _params 
                  blockHeight = LATEST }
                |> contract.env.connection)
        |> Result.bind (fun r ->
            unpackRoot r
            |> stringAndTrim
            |> EthTransactionHash
            |> fun ethHash ->
                contract.env.log (Library $"Monitoring transaction {ethHash}" |> Ok )
                |> fun _ -> ethHash
            |> Ok)        
        |> monitorTransaction contract.env.monitor
        
        
        
    ///
    /// Estimate the amount of gas units required for the given transaction to
    /// complete. Essentially the same as contractTransaction but with a
    /// different underlying call. A static value argument of "0" is supplied.
    ///
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionSelector corresponding to the the function being
    ///     called. Typically (ByName "SomeFunction")
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no
    ///     arguments.
    /// * env: An Web3Environment. See createWeb3Environment.
    let public estimateGas contract evmFunction arguments env =
        let blockHeight' = LATEST
        
        () |> Ok
        |> checkFunctionExists contract evmFunction
        |> createUnvalidatedTxn env contract arguments "0"
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.EstimateGas 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> env.connection)
        |> decomposeRPCResult EthMethod.EstimateGas
        

    ///
    /// This function is for the sending of Ether between Externally Owned
    /// Accounts. Use 'contractTransaction' with the `Receive` indicator to send
    /// to contracts. ENS names are supported for this function.
    ///
    let public sendValue chainId destination value env = 
        let _dest = handleENSName env chainId destination
        { dummyTransaction with
            utoAddr = _dest
            ufrom =  env.signerAddress
            uvalue = value |> bigintToHex
            uchainId = chainId}
        |> validateRPCParams
        |> Result.bind
            (fun _params ->
                {method = EthMethod.SendTransaction
                 paramList = _params
                 blockHeight = LATEST}
                |> env.connection)
        |> Result.bind (fun r ->
            unpackRoot r
            |> stringAndTrim
            |> EthTransactionHash
            |> fun ethHash ->
                env.log (Library $"Monitoring transaction {ethHash}" |> Ok )
                |> fun _ -> ethHash
            |> Ok)        
        |> monitorTransaction env.monitor
        
    ///
    /// Expose every single function directly
    /// Now we can easily check for argument correctness where we couldn't before
    /// .estimateGas (FunctionSelector -> EVMDatatype list -> DeployedContract -> Result<CallResponses, Web3Error>)
    /// .sendValue (env -> ETHAddress -> Wei -> Result<CallResponses, Web3Error>)
    /// .everySingleEthMethod
    // type RPC = class end
    //     with
    //     static member getChainId = ()
    

    /// .dumpStorage (DeployedContract -> ...?)
    /// .getLogByTopic (DeployedContract -> ...?)
    /// .getSourceCode ...
    type Contract = class end
        with    
        
        static member prepare env arguments abiAndBytecode =
            prepareUndeployedContract env arguments abiAndBytecode
            
        static member deploy value contract =
            deployContract value contract
        
        static member connect env abi address =
            loadDeployedContract env abi address
        
        static member call function' arguments contract =
            contractCall function' arguments contract
        
        static member tx function' arguments value contract =
            contractTransaction function' arguments value contract
        
        static member fallback arguments value contract =
          contractTransaction Fallback arguments value contract
          
        static member receive value contract =
            contractTransaction Receive value contract
            
        static member listFunctions contract =
            contract.functions
            |> List.iter (fun p -> contract.env.log (Library $"{p}" |> Ok) |> ignore)
            
        static member listEvents contract =
            contract.events
            |> List.iter (fun p -> contract.env.log (Library $"{p}" |> Ok) |> ignore)
            
        static member listErrors contract =
            contract.errors
            |> List.iter (fun p -> contract.env.log (Library $"{p}" |> Ok) |> ignore)
        
        