namespace web3.fs

[<AutoOpen>]    
module Logging =
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Logging
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Generic logger until I import something more featureful.
    let internal logResult logString =
        printfn $"{logString}"
        
    
    ///
    /// Binds and starts the transaction monitor if a transaction hash was emitted from `makeEthTxn`. Intended to be
    /// placed in a transaction pipeline to provide realtime logging of transaction completion.
    /// 
    let internal monitorTransaction (monitor: Monitor) (r: Result<EthTransactionHash, Web3Error>) =
        match r with
        | Ok o ->
            logResult $"Monitoring transaction {o}..."
            monitor o 
        | Error e -> e |> Error 
        
    
    ///
    /// Handles the emission of information to the console
    let private logCallResponse callResponse =
        match callResponse with
        | SimpleValue s -> printfn $"Value:\n{s}"
        | Block ethBlock -> printfn $"Ethereum block:\n{ethBlock}"
        | TransactionHash _ -> () // Handled by the monitor
        | TransactionReceiptResult rpcTransactionResponse -> printfn $"Transaction receipt:\n{rpcTransactionResponse}"
        | Transaction mTransaction -> printfn $"Transaction:\n{mTransaction}"
        | CallResult callResult -> printfn $"Call result:\n{callResult}"
        | Empty -> () // Do nothing
        
        
    ///
    /// Forwards CallResponses, logs errors to the console
    let private logCallResponsesOrWeb3Errors pipeResult =
        match pipeResult with
        | Ok o -> logCallResponse o
        | Error e -> printfn $"Error:\n{e}"
    
    
    ///
    /// Unwraps Result for the logging mechanism when Emit or LogAndEmit are signalled.
    let private emitter pipeResult =
        match pipeResult with
        | Ok callResponses -> callResponses
        | Error _ -> Empty
    
    
    ///
    /// Generic logger for use in all RPC calls. Takes a signal to indicate whether the user wants just a log to console,
    /// to emit a wrapped record, or both.
    /// 
    let public log signal pipeResult =
        match signal with
        | Log ->
            logCallResponsesOrWeb3Errors pipeResult
            Empty
        | Emit -> emitter pipeResult
        | LogAndEmit ->
            logCallResponsesOrWeb3Errors pipeResult
            emitter pipeResult
        | Quiet -> Empty
    
    