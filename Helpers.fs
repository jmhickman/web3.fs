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
        
        
    