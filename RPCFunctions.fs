namespace web3.fs

//
// RPC method module
//

module RPCMethodFunctions =
    open Types


    //
    // lifters
    //


    let wrapEthMethod m = m |> EthMethod
    let wrapShhMethod m = m |> ShhMethod
    let wrapNetMethod m = m |> NetMethod
    let wrapWeb3Method m = m |> Web3Method


    //
    // Convert calltype into json string representation
    //


    let bindEthMethod (m: EthMethod) =
        match m with
        | EthMethod.Accounts -> "eth_accounts"
        | EthMethod.BlockNumber -> "eth_blockNumber"
        | EthMethod.Call -> "eth_call"
        | EthMethod.Coinbase -> "eth_coinbase"
        | EthMethod.CompileLLL -> "eth_compileLLL"
        | EthMethod.CompileSerpent -> "eth_compileSerpent"
        | EthMethod.CompileSolidity -> "eth_compileSolidity"
        | EthMethod.EstimateGas -> "eth_estimateGas"
        | EthMethod.GasPrice -> "eth_gasPrice"
        | EthMethod.GetBalance -> "eth_getBalance"
        | EthMethod.GetBlockByHash -> "eth_getBlockByHash"
        | EthMethod.GetBlockByNumber -> "eth_getBlockByNumber"
        | EthMethod.GetBlockTransactionCountByHash -> "eth_getBlockTransactionCountByHash"
        | EthMethod.GetBlockTransactionCountByNumber -> "eth_getBlockTransactionCountByNumber"
        | EthMethod.GetCode -> "eth_getCode"
        | EthMethod.GetCompilers -> "eth_getCompilers"
        | EthMethod.GetFilterChanges -> "eth_getFilterChanges"
        | EthMethod.GetFilterLogs -> "eth_getFilterLogs"
        | EthMethod.GetLogs -> "eth_getLogs"
        | EthMethod.GetStorageAt -> "eth_getStorageAt"
        | EthMethod.GetTransactionCount -> "eth_getTransactionCount"
        | EthMethod.GetTransactionByHash -> "eth_getTransactionByHash"
        | EthMethod.GetTransactionByBlockHashAndIndex -> "eth_getTransactionByBlockHashAndIndex"
        | EthMethod.GetTransactionByBlockNumberAndIndex -> "eth_getTransactionByBlockNumberAndIndex"
        | EthMethod.GetTransactionReceipt -> "eth_getTransactionReceipt"
        | EthMethod.GetUncleByBlockHashAndIndex -> "eth_getUncleByBlockHashAndIndex"
        | EthMethod.GetUncleByBlockNumberAndIndex -> "eth_getUncleByBlockNumberAndIndex"
        | EthMethod.GetUncleCountByBlockHash -> "eth_getUncleCountByBlockHash"
        | EthMethod.GetUncleCountByBlockNumber -> "eth_getUncleCountByBlockNumber"
        | EthMethod.GetWork -> "eth_getWork"
        | EthMethod.Hashrate -> "eth_hashrate"
        | EthMethod.Mining -> "eth_mining"
        | EthMethod.NewFilter -> "eth_newFilter"
        | EthMethod.NewBlockFilter -> "eth_newBlockFilter"
        | EthMethod.NewPendingTransactionFilter -> "eth_newPendingTransactionFilter"
        | EthMethod.ProtocolVersion -> "eth_protocolVersion"
        | EthMethod.Syncing -> "eth_syncing"
        | EthMethod.SendTransaction -> "eth_sendTransaction"
        | EthMethod.SendRawTransaction -> "eth_sendRawTransaction"
        | EthMethod.Sign -> "eth_sign"
        | EthMethod.SignTransaction -> "eth_signTransaction"
        | EthMethod.SubmitWork -> "eth_submitWork"
        | EthMethod.SubmitHashRate -> "eth_submitHashRate"
        | EthMethod.UninstallFilter -> "eth_uninstallFilter"

    // placeholder
    let bindNetMethod m = ""

    // placeholder
    let bindShhMethod m = ""

    // placeholder
    let bindWeb3Method m = ""

    let bindRPCMethod method =
        match method with
        | EthMethod m -> bindEthMethod m
        | NetMethod m -> bindNetMethod m
        | ShhMethod m -> bindShhMethod m
        | Web3Method m -> bindWeb3Method m


//
// RPC Parameter module
//


module RPCParamFunctions =
    open Types
    open Helpers


    //
    // lifters
    //


    let wrapEthParams p = p |> EthParam
    let wrapNetParams p = p |> NetParam
    let wrapWeb3Params p = p |> Web3Param


    //
    // Convert call params into json string representation. Some params
    // will apparently be deprecated soon, if not already. Log types
    // not yet implemented.
    //

    let bindEthParam (p: EthParam) =
        match p with
        | EthParamAccounts _ -> ""
        | EthParamBlockNumber _ -> ""
        | EthParamCoinbase _ -> ""
        //| EthParamCompileLLL p -> concatParamString p
        //| EthParamCompileSerpent p -> concatParamString p
        //| EthParamCompileSolidity p -> concatParamString p
        | EthParamGasPrice _ -> ""
        | EthParamGetBalance p -> concatParamString p
        | EthParamGetBlockByHash p -> concatParamString p
        | EthParamGetBlockByNumber p -> concatParamString p
        | EthParamGetBlockTransactionCountByHash p -> concatParamString p
        | EthParamGetBlockTransactionCountByNumber p -> concatParamString p
        | EthParamGetCode p -> concatParamString p
        //| EthParamGetCompilers p ->
        | EthParamGetFilterChanges p -> concatParamString p
        | EthParamGetFilterLogs p -> concatParamString p
        | EthParamGetLogs p -> "" // not implemented
        | EthParamGetStorageAt p -> concatParamString p
        | EthParamGetTransactionCount p -> concatParamString p
        | EthParamGetTransactionByHash p -> concatParamString p
        | EthParamGetTransactionByBlockHashAndIndex p -> concatParamString p
        | EthParamGetTransactionByBlockNumberAndIndex p -> concatParamString p
        | EthParamGetTransactionReceipt p -> concatParamString p
        | EthParamGetUncleByBlockHashAndIndex p -> concatParamString p
        | EthParamGetUncleByBlockNumberAndIndex p -> concatParamString p
        | EthParamGetUncleCountByBlockHash p -> concatParamString p
        | EthParamGetUncleCountByBlockNumber p -> concatParamString p
        | EthParamGetWork _ -> ""
        | EthParamHashrate _ -> ""
        | EthParamMining _ -> ""
        | EthParamNewFilter p -> "" // not implemented
        | EthParamNewBlockFilter p -> concatParamString p
        | EthParamNewPendingTransactionFilter _ -> ""
        | EthParamProtocolVersion _ -> ""
        | EthParamSyncing _ -> ""
        | EthParamSendRawTransaction p -> concatParamString p
        | EthParamSign p -> concatParamString p
        | EthParamSubmitWork p -> concatParamString p
        | EthParamSubmitHashRate p -> concatParamString p
        | EthParamUninstallFilter p -> concatParamString p
        | _ -> "" //not great


    let bindRPCParam p =
        match p with
        | EthParam e ->
            match e with
            | EthParam1559Call _e -> createJsonObj _e
            | EthParam1559EstimateGas _e -> createJsonObj _e
            | EthParam1559SendTransaction _e -> createJsonObj _e
            | EthParam1559SignTransaction _e -> createJsonObj _e
            | _ -> bindEthParam e
        | NetParam e -> ""
        | Web3Param e -> ""
