namespace web3.fs

open Types

[<AutoOpen>]    
module Logging =
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Logging
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Binds and starts the transaction monitor if a transaction hash was
    /// emitted from `makeEthTxn`. Intended to be placed in a transaction
    /// pipeline to provide realtime logging of transaction completion.
    /// 
    let internal monitorTransaction (monitor: Monitor) (r: Result<EthTransactionHash, Web3Error>) =
        match r with
        | Ok o -> monitor o 
        | Error e -> e |> Error 
        
    
    ///
    /// Forwards CallResponses, logs errors to the console
    let private logCallResponsesOrWeb3Errors (logger: Logger) (pipeResult: Result<CallResponses, Web3Error>) =
        match pipeResult with
        | Ok o -> logger.Post (Success, o)
        | Error e ->
            match e with
            | PayableFunctionZeroValueWarning w -> logger.Post (Warn, $"{w}" |> Library)
            | e -> logger.Post (Failure, $"{e}" |> Library)
    
    
    ///
    /// Unwraps Result when Emit or LogAndEmit are signalled.
    let private emitter pipeResult =
        match pipeResult with
        | Ok callResponses -> callResponses
        | Error _ -> Empty
    
    
    ///
    /// Generic logger for use in all RPC calls.
    /// 
    /// * logger: A `Logger`. Typically generated from `createWeb3Environment`.
    /// * signal: An indicator to tell the logger how to handle the Result. One
    ///           of 'Log', 'Emit', 'LogAndEmit', or 'Quiet'
    /// * pipeResult: Result flow from an Ethereum call into this function.
    /// 
    let public log (logger: Logger) signal (pipeResult: Result<CallResponses, Web3Error>) =
        match signal with
        | Log ->
            logCallResponsesOrWeb3Errors logger pipeResult
            Async.Sleep(100) |> Async.RunSynchronously // I don't like this bodge
            Empty
        | Emit -> emitter pipeResult
        | LogAndEmit ->
            logCallResponsesOrWeb3Errors logger pipeResult
            Async.Sleep(100) |> Async.RunSynchronously
            emitter pipeResult
        | Quiet -> Empty
                