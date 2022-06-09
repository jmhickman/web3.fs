namespace web3.fs

open Types

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
    // Builder functions for creating unvalidated transaction objects
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
    
    
    ///
    /// Detects if an ENS name was used and performs the lookup to resolve it to an address
    let handleENSName env chainId (name: string) =
        if name.Contains('.') then
            let hash = convertENSName name
            let bytes = [Byte32 hash] |> createInputByteString |> function Ok o -> o | Error _ -> "" 
            let evmData = "02571be3" + bytes |> prepend0x
            { utxnType = "0x2"
              unonce = ""
              utoAddr = "0x00000000000C2E074eC69A0dFb2997BA6C7d2e1e"
              ufrom = env.constants.walletAddress
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
    /// Get constants values out of the ContractConstants
    let private unpackConstants (constants: ContractConstants) =
        constantsBind constants |> (fun a -> a |> Ok)
        
    
    ///
    /// Convert the supplied value into hex form
    let private convertValueToHex value (pipe: Result<string * string * string * EVMDatatype list, Web3Error>) =
        pipe
        |> Result.bind(fun (a, b, c, d) ->
            (a, b, c, d, (value |> bigintToHex |> prepend0x))
            |> Ok)
        
    
    ///
    /// Places the actual EVMFunction into the data for later use
    let private pipeBindFunction evmFunction (pipe: Result<string * string * string * EVMDatatype list * string, Web3Error>) =
        pipe
        |> Result.bind(fun (a, b, c, d, e) -> (a, b, c, d, e, (bindFunctionIndicator evmFunction)) |> Ok)
        
    
    ///
    /// Checks that we aren't trying to call the fallback or receive function(s) on a contract that doesn't have them.
    let checkForFallbackOrReceive contract (pipe: Result<string * string * string * EVMDatatype list * string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind(fun (_,_,_,_,_, requestedFunction) ->
            if requestedFunction.name = "fallback" && contract.hasFallback = false then
                ContractLacksFallbackError |> Error
            else if requestedFunction.name = "receive" && contract.hasReceive = false then
                ContractLacksReceiveError |> Error
            else pipe)
    
    ///
    /// Selects the supplied arguments or ones defaulted in the ContractConstants.
    let private chooseDefaultOrSuppliedArguments suppliedArgs (pipe: Result<string * string * string * EVMDatatype list * string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind(fun (a, b, c, defaultArgs, e, f) ->
            match suppliedArgs with
            | [] -> (a, b, c, defaultArgs, e, f) |> Ok
            | s -> (a, b, c, s, e, f) |> Ok)
    
    
    ///
    /// Checks that we are lined up as far as function signature and supplied arguments.
    let private checkArgsToFunction (pipe: Result<string * string * string * EVMDatatype list * string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind(fun a ->
            let _, _, _, args, _, evmFunction = a
            match evmFunction.canonicalInputs with
            |x when bindEVMFunctionInputs x = "()" ->
                if args.Length = 0 then a |> Ok
                else ArgumentsToEmptyFunctionSignatureError |> Error
            | _ ->                
                if not(args.Length = 0) then a |> Ok
                else FunctionArgumentsMissingError |> Error )
        
    
    ///
    /// Run arguments through checkEVMData
    let private checkArgumentData (pipe: Result<string * string * string * EVMDatatype list * string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind (fun a ->
            let _, _, _, args, _, _ = a
            match checkEVMData args with
            | Ok _ -> a |> Ok
            | Error e -> e |> Error)
    
    
    ///
    /// Check that we're not sending value to a non-Payable, or warn if 0 to a Payable
    let private checkValueAndStateMutability (pipe: Result<string * string * string * EVMDatatype list * string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind (fun a ->
            let _, _, _, _, value, evmFunction = a
            match evmFunction.config with
            | Payable -> a |> Ok
            | _ ->
                if not(value = "0x0") then
                    ValueToNonPayableFunctionError |> Error
                else a |> Ok )
    
    
    ///
    /// Create the 'data' value string from arguments and function
    let private createDataString (pipe: Result<string * string * string * EVMDatatype list * string * EVMFunction, Web3Error>) =
        pipe
        |> Result.bind (fun (a, b, c, args, e, evmFunction) ->
            createInputByteString args
            |> Result.bind (fun r -> (a, b, c, e, $"{evmFunction.hash}{r}") |> Ok)) 
            
            
        
    
    ///
    /// Creates an unvalidated record 
    let private returnUnvalidatedRecord address contract (pipe: Result<string * string * string * string * string, Web3Error>)  =
        pipe
        |> Result.bind (fun (txn, maxfee, priority, value, data) -> 
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
          uchainId = contract.chainId } |> Ok)
        
    
    ///
    /// Returns a Txn object for use in the validation function `ValidateRPCParams`
    let private createUnvalidatedTxn constants contract arguments value (pipe: Result<FunctionIndicator, Web3Error>) =
        pipe
        |> Result.bind (fun functionIndicator ->
            constants
            |> unpackConstants
            |> convertValueToHex value
            |> pipeBindFunction functionIndicator
            |> checkForFallbackOrReceive contract 
            |> chooseDefaultOrSuppliedArguments arguments
            |> checkArgsToFunction
            |> checkArgumentData
            |> checkValueAndStateMutability
            |> createDataString
            |> returnUnvalidatedRecord constants.walletAddress contract )

    
    ///
    /// Factored out for reuse. Passes through a specified blockheight, or supplies the LATEST default.
    let private blockHeight (constants: ContractConstants) =
        match constants.blockHeight with
        | Some s -> s
        | None -> LATEST
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Deployment Function Helpers
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
          |> env.log Emit
          |> unwrapSimpleValue
          |> fun chain ->
                if chain = chainId then
                    () |> Ok
                else WrongChainInSignerError |> Error
    
    
    ///
    /// Checks that the value string isn't just empty "".
    let private checkForEmptyValueString (value: string) (pipe: Result<'a, Web3Error>) =
        let bool, _ = bigint.TryParse(value)
        if bool then pipe else InvalidValueArgumentError |> Error
    
    
    ///
    /// Checks that the function we're attempting to call exists.
    let private checkFunctionExists contract functionSelector (pipe: Result<unit, Web3Error>) =
        pipe
        |> Result.bind (fun _ -> findFunction contract functionSelector)
        
    
    ///
    /// Unpacks constants from the ContractConstants record
    let private unpackDeployConstants constants (pipe: Result<'a, Web3Error>) =
        pipe
        |> Result.bind (fun _ ->
            constantsBind constants |> (fun (a, b, c, _) -> (a, b, c) |> Ok))


    ///
    /// Check that we aren't supplying value to a non-Payable constructor, which will be accepted but the transaction
    /// will fail with a status 0x0.
    /// 
    let private checkValueAndStateMutabilityDeploy value contract (pipe: Result<string * string * string, Web3Error>) =
        match contract.stateMutability with
        | Payable -> pipe
        | _ -> if value = "0" then pipe else ValueToNonPayableFunctionError |> Error
    
    
    ///
    ///
    let private createDeployData args (pipe: Result<string * string * string,Web3Error>) =
        pipe
        |> Result.bind (fun (a, b, c) ->
            match createInputByteString args with
            | Ok data -> (a, b, c, data) |> Ok
            | Error e -> e |> Error)
            
    
    ///
    /// Creates the unvalidated call record specifically for deployment.
    let private buildDeploymentCall env value contract (pipe: Result<string * string * string * string, Web3Error>) =
        let (RawContractBytecode _rawBytecode) = contract.bytecode
        if contract.stateMutability = Payable && value = "0" then
            env.log Log (PayableFunctionZeroValueWarning "Zero value sent to payable function" |> Error) |> ignore
        
        pipe
        |> Result.bind (fun (txn, maxfee, priority, data) ->
            { utxnType = txn
              unonce = ""
              utoAddr = ""
              ufrom = env.constants.walletAddress
              ugas = "0x4C4B40"
              uvalue = value |> bigintToHex |> prepend0x
              udata =
                  $"{_rawBytecode}{data}" |> prepend0x           
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
    /// * method: An EthMethod selector, like `EthMethod.GetBalance`
    /// * paramList: An EthParam, such as `["0x3872353821064f55df53ad1e2d7255e969f6eac0", "0x9dc3fe"]`
    /// * chainId: The hexadecimal representation of the chain ID.
    /// * env: A Web3Environment. See createWeb3Environment.
    /// Note that some EthMethods have no arguments. Use an empty list in those cases.
    ///
    let public rpcCall method (paramList: string list) chainId env =
        checkForChain env chainId
        |> Result.bind ( fun _ ->
        { method = method
          paramList = paramList |> EthGenericRPC
          blockHeight = LATEST }
        |> env.connection
        |> decomposeRPCResult method)


    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain).
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionSelector corresponding to the the function being called. Typically (ByName "SomeFunction")
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no arguments.
    /// * value: the wei-denominated amount of ETH to send along with a transaction to a payable function.
    /// * env: An Web3Environment. See createWeb3Environment.
    ///
    let public contractTransaction contract evmFunction arguments value env =
        checkForChain env contract.chainId
        |> checkForEmptyValueString value
        |> checkFunctionExists contract evmFunction 
        |> createUnvalidatedTxn env.constants contract arguments value
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
            |> fun ethHash ->
                env.log Log (Library $"Monitoring transaction {ethHash}\n" |> Ok )
                |> fun _ -> ethHash
            |> Ok)
        |> monitorTransaction env.monitor
        
    
    
    ///
    /// Creates an Ethereum call that does NOT change the state of the blockchain.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionSelector corresponding to the the function being called. Typically (ByName "SomeFunction")
    /// * arguments: a list of EVMDatatypes. Use an empty list to indicate no arguments.
    /// * env: An Web3Environment. See createWeb3Environment.
    ///
    let public contractCall contract evmFunction arguments env =
        let blockHeight' = blockHeight env.constants
        checkForChain env contract.chainId
        |> checkFunctionExists contract evmFunction
        |> createUnvalidatedTxn env.constants contract arguments "0"  
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
    /// * contract: A UndeployedContract that is being deployed
    /// * value: the wei-denominated amount of ETH to send along with a transaction to a payable constructor.
    /// * env: An Web3Environment. See createWeb3Environment.
    ///
    let public deployContract (contract: UndeployedContract) value env =
        checkForChain env contract.chainId
        |> checkForEmptyValueString value
        |> unpackDeployConstants env.constants
        |> checkValueAndStateMutabilityDeploy value contract
        |> createDeployData contract.constructorArguments 
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
            |> fun ethHash ->
                env.log Log (Library $"Monitoring transaction {ethHash}" |> Ok )
                |> fun _ -> ethHash
            |> Ok)        
        |> monitorTransaction env.monitor
        
        
        
    ///
    /// Estimate the amount of gas units required for the given transaction to complete. Essentially the same as 
    /// `makeEthTxn` but with a different underlying call. A static value argument of "0" is supplied.
    let public estimateGas contract evmFunction arguments env =
        let blockHeight' = blockHeight env.constants
        
        checkForChain env contract.chainId
        |> checkFunctionExists contract evmFunction
        |> createUnvalidatedTxn env.constants contract arguments "0"
        |> Result.bind validateRPCParams
        |> Result.bind
            (fun _params ->
                { method = EthMethod.EstimateGas 
                  paramList = _params 
                  blockHeight = blockHeight' }
                |> env.connection)
        |> decomposeRPCResult EthMethod.EstimateGas
        

    ///
    /// This function is for the sending of Ether between EOAs. Use `makeEthTxn` with the `Receive` function indicator
    /// to send to contracts. ENS names are supported for this function.
    ///
    let public sendValue chainId destination value env = 
        let _dest = handleENSName env chainId destination
        { dummyTransaction with
            utoAddr = _dest
            ufrom =  env.constants.walletAddress
            uvalue = value |> bigintToHex |> prepend0x
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
                env.log Log (Library $"Monitoring transaction {ethHash}" |> Ok )
                |> fun _ -> ethHash
            |> Ok)        
        |> monitorTransaction env.monitor