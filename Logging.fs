namespace web3.fs

open web3.fs.Types

[<AutoOpen>]    
module Logging =
    
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Logging
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Binds and starts the transaction monitor if a transaction hash was emitted from `makeEthTxn`. Intended to be
    /// placed in a transaction pipeline to provide realtime logging of transaction completion.
    /// 
    let internal monitorTransaction (monitor: Monitor) (r: Result<EthTransactionHash, Web3Error>) =
        match r with
        | Ok o ->
            monitor o 
        | Error e -> e |> Error 
        
    
    ///
    /// Handles the emission of information to the console
    let private logCallResponse (logger: Logger) callResponse =
        match callResponse with
        | SimpleValue s -> logger.Post (Success, $"Value: {s}")
        | Block ethBlock -> logger.Post (Info, $"Ethereum block:\n{ethBlock}")
        | TransactionHash _ -> () // Handled by the monitor
        | TransactionReceiptResult rpcTransactionResponse -> logger.Post (Info, $"Transaction receipt:\n{rpcTransactionResponse}")
        | Transaction mTransaction -> logger.Post (Info, $"Transaction:\n{mTransaction}")
        | CallResult callResult -> logger.Post (Success, $"Call result: {callResult}")
        | Library s -> logger.Post (Info, s)
        | Empty -> () // Do nothing
        
        
    ///
    /// Forwards CallResponses, logs errors to the console
    let private logCallResponsesOrWeb3Errors (logger: Logger) (pipeResult: Result<CallResponses, Web3Error>) =
        match pipeResult with
        | Ok o -> logCallResponse logger o
        | Error e ->
            match e with
            | PayableFunctionZeroValueWarning w -> logger.Post (Warn, w)
            | e -> logger.Post (Failure, $"{e}")
    
    
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
    let public log (logger: Logger) signal (pipeResult: Result<CallResponses, Web3Error>) =
        match signal with
        | Log ->
            logCallResponsesOrWeb3Errors logger pipeResult
            Async.Sleep(100) |> Async.RunSynchronously
            Empty
        | Emit -> emitter pipeResult
        | LogAndEmit ->
            logCallResponsesOrWeb3Errors logger pipeResult
            Async.Sleep(100) |> Async.RunSynchronously
            emitter pipeResult
        | Quiet -> Empty
                