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
module RPCBindFunctions =
    
    open FSharp.Data
    
    open Helpers
    
    
    ///
    /// Generic logger until I import something more featureful.
    let internal logResult logString =
        printfn $"{logString}"
        
           
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
    /// Binds and starts the transaction monitor if a transaction hash was emitted from `makeEthTxn`. Intended to be
    /// placed in a transaction pipeline to provide realtime logging of transaction completion.
    /// 
    let public monitorTransaction (monitor: Monitor) (r: Result<EthTransactionHash,Web3Error>) =
        r
        |> Result.bind (fun r ->
            printfn $"Beginning monitoring of transaction {r}"
            monitor r)
        
    
    ///
    /// Unpacks Result from a RPCResponse.Root for logging
    let internal unpackRoot (r:RPCResponse.Root) =
        match r.Result with
        | Some r' -> r'
        | None -> RPCResponse.Result(JsonValue.Null)
    
    
    ///
    /// Higher order function that applies functions to generic RPCResponses in order to extract specific types
    /// (CallResponses). Intended to be used with the set of `binder_` functions, but can be composed with a lambda if
    /// required.
    /// 
    let private bindCallResult (f: RPCResponse.Root -> CallResponses) (r:Result<RPCResponse.Root,Web3Error>) =
        match r with
        | Ok r' -> r' |> f |> Ok
        | Error e -> e |> Error
    
    
    ///
    /// Creates a TransactionReceiptResult from an incoming RPCResponse. Intended to be paired with `bindCallResult`.
    let private binderTransactionResult (r: RPCResponse.Root) =
        match r.Result with
        | Some r' -> 
            { blockHash = r'.BlockHash
              blockNumber = r'.BlockNumber
              contractAddress = r'.ContractAddress.JsonValue.ToString() |> trimParameter |> Some
              cumulativeGasUsed = r'.CumulativeGasUsed
              effectiveGasPrice = r'.EffectiveGasPrice
              from = r'.From
              gasUsed = r'.GasUsed
              logs = r'.Logs |> Array.map (fun l -> l.JsonValue.ToString()) |> Array.toList
              logsBloom = r'.LogsBloom
              status = r'.Status
              toAddr = r'.To.JsonValue.ToString() |> EthAddress
              transactionHash = r'.TransactionHash
              transactionIndex = r'.TransactionIndex
              tType = r'.Type }
            |> TransactionReceiptResult 
        | None -> Null

    
    let private binderGetTransactionByHashResult (r: RPCResponse.Root) =
        match r.Result with
        | Some m' ->
            let m = RPCMinedTransaction.Parse(m'.ToString())
            { accessList = m.AccessList |> Array.map (fun l -> l.JsonValue.ToString()) |> Array.toList
              blockHash = m.BlockHash
              blockNumber = m.BlockNumber
              chainId = m.ChainId
              from = m.From |> EthAddress
              gas = m.Gas
              gasPrice = m.GasPrice
              hash = m.Hash |> EthTransactionHash
              input = m.Input
              maxFeePerGas = m.MaxFeePerGas
              maxPriorityFeePerGas = m.MaxPriorityFeePerGas
              nonce = m.Nonce
              r = m.R
              s = m.S
              toAddr = m.To |> EthAddress
              transactionIndex = m.TransactionIndex
              tType = m.Type
              v = m.V
              value = m.Value }
            |> Transaction
        | None ->  Null 
    
    
    let internal bindTransactionResult = bindCallResult binderTransactionResult
    let internal bindGetTransactionByHashResult = bindCallResult binderGetTransactionByHashResult
