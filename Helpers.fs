namespace Web3.fs

[<AutoOpen>]
module Helpers =

    open Logger
    
    let public ZEROVALUE = "0" |> Wei
        
    ///
    /// Convenience function that creates all of the necessary parts of a
    /// functioning web3 environment. The record contains the rpc connection, a
    /// transaction monitor, contract constants, a Keccak digester, and the
    /// initialized logger instance.
    /// 
    let public createWeb3Environment url chainId address =
        let rpc = createWeb3Connection url
        let log' = startLogger() |> log
        {connection = rpc
         chainId = chainId
         monitor = createReceiptMonitor rpc
         signerAddress = address
         maxFeePerGas = "0" |> Wei |> Some
         maxPriorityFeePerGas = "0" |> Wei |> Some
         log = log' Log
         emit = log' Emit
         logAndEmit = log' LogAndEmit
         quiet = log' Quiet }
        
    
    ///
    /// Sometimes crafting or exposing bytestrings can be used programmatically,
    /// such as to create calldata for use in certain functions.
    /// 
    let public returnEVMBytestring evmDatatypes =
        match createInputByteString evmDatatypes with
        | Ok resultValue -> resultValue
        | Error e -> $"{e}"
        

    ///
    /// Because of the potential for errors during contract loading,
    /// `loadDeployedContract` returns a Result type. The Result is bound into
    /// a singleton list in the success case. This function returns the
    /// DeployedContract on its own, ready for use. However, its use of
    /// `List.head` means that it will crash the library if you use it when the
    /// contract won't parse properly or have some other Web3Error.
    /// 
    let public optimisticallyBindDeployedContract a =
        a |> bindDeployedContract |> List.head
    
    
    
    ///
    /// Combines the preparation and deployment of a contract. Automatically
    /// calls Log on environment logger. Mostly for convenience.
    /// 
    let public prepareAndDeployContract bytecode abi chainId (constructorArguments: EVMDatatype list) value env =
        prepareUndeployedContract bytecode abi chainId constructorArguments 
        |> Result.bind(fun contract -> deployContract contract value env )
        |> env.log
        |> fun _ -> ()
    
    
    ///
    /// Combines the preparation, deployment, and loading steps of contract
    /// interaction. Mostly for convenience.
    /// 
    // let public prepareDeployAndLoadContract bytecode abi chainId (constructorArguments: EVMDatatype list) value env =
    //     prepareUndeployedContract bytecode abi chainId constructorArguments
    //     |> Result.bind(fun contract -> deployContract contract value env)
    //     |> Result.bind(fun res -> 
    //         match res with
    //         | TransactionReceiptResult transactionReceipt ->
    //             loadDeployedContract abi chainId transactionReceipt.contractAddress.Value
    //         | _ -> "Result of `deployEthContract` wasn't of the expected type" |> GenericPipelineError |> Error )


    ///
    /// Public accessor for a bind function to get the simple string out of
    /// contract function wrapper type. Use with
    /// <contract>.functions.canonicalInputs
    /// 
    let public unwrapFunctionInputs evmInputs =
        bindEVMFunctionInputs evmInputs
        
        
    ///
    /// Public accessor for a bind function to get the simple string out of a
    /// contract function wrapper type. Use with
    /// <contract>.functions.canonicalOutputs
    /// 
    let public unwrapFunctionOutputs evmOutputs =
        bindEVMFunctionOutputs evmOutputs
        
        
    