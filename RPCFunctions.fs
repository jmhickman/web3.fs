namespace web3.fs

///
/// Helpers for library-wide functionality
///

module Helpers =
    open System.Globalization
    open System.Text.RegularExpressions

    open Types
    open FSharp.Json


    ///
    /// validators for the RPC data formats QUANTITY, DATA, TAG, ADDRESS
    ///


    let validateTxnType t =
        let reg = new Regex("^0x([0-9,a-f,A-F]){1,2}$")

        match reg.Match(t).Success with
        | true -> Some t
        | false -> None

    let validateQuantity s =
        let reg = new Regex("^0x([1-9a-f]+[0-9a-f]*|0)$")

        match reg.Match(s).Success with
        | true -> Some s
        | false -> None

    let validateData s =
        let reg = new Regex("^0x([0-9a-f]{2})*$")

        match reg.Match(s).Success with
        | true -> Some s
        | false -> None

    let validateAddress s =
        let reg = new Regex("^0x[0-9,a-f,A-F]{40}$")

        match reg.Match(s).Success with
        | true -> Some s
        | false -> None

    // The purpose of this function isn't to construct calls/txns that will
    // always be accepted by the node. Rather, it is to ensure that data
    // passed into a call/txn is internally consistent with the expected
    // hex formats. Only data has an explicit check, since any valid call
    // must include something in the data field. Misformatted inputs will be
    // converted to None (which will be omitted on serialization).
    let validateRPCParams (unvalidatedRpcParam: UnvalidatedEthParam1559Call) =
        match validateData unvalidatedRpcParam.udata with
        | Some d ->
            { txnType = validateTxnType unvalidatedRpcParam.utxnType
              nonce = validateQuantity unvalidatedRpcParam.unonce
              toAddr = validateAddress unvalidatedRpcParam.utoAddr
              from = validateAddress unvalidatedRpcParam.ufrom
              gas = validateQuantity unvalidatedRpcParam.ugas
              value = validateQuantity unvalidatedRpcParam.uvalue
              maxFeePerGas = validateQuantity unvalidatedRpcParam.umaxFeePerGas
              maxPriorityFeePerGas = validateQuantity unvalidatedRpcParam.umaxPriorityFeePerGas
              chainId = validateQuantity unvalidatedRpcParam.uchainId
              accessList = unvalidatedRpcParam.uaccessList |> Some
              data = d }
            |> EthParam1559Call
            |> Ok
        | None ->
            "Call/TXN object 'data' value is missing or not valid"
            |> Error


    ///
    /// Parameter list handlers
    ///


    let jsonConfig =
        JsonConfig.create (serializeNone = SerializeNone.Omit, unformatted = true)

    /// Create a serialized Json representation of call
    let createJsonObj (ethParams: EthParam1559Call) = Json.serializeEx jsonConfig ethParams

    /// For 'dumb' list-style parameter types.
    let concatParamString (list: string list) =
        list
        |> List.fold (fun acc s -> $"""{acc}"{s}", """) ""
        |> fun s -> s.TrimEnd(',', ' ')


    ///
    /// Hex and bigint functions
    ///


    let prepend0x s = "0x" + s

    let strip0x (s: string) =
        if s.StartsWith("0x") then
            s.Remove(0, 2)
        else
            s

    /// Based on blessed code found at https://stu.dev/bigint-to-string-in-any-base-fsharp/
    let bigintToIntList _base input =
        let rec loop (_base: int) input digits =
            let (quotient, remainder) = bigint.DivRem(input, bigint _base)

            match quotient with
            | zero when zero = 0I -> int remainder :: digits
            | _ -> loop _base quotient (int remainder :: digits)

        loop _base input []

    // partial application for hexadecimal
    let bigintToHexList = bigintToIntList 16

    let intListToString input =
        input
        |> List.fold (fun acc (x: int) -> $"""{acc}{x.ToString("X").ToLower()}""") ""

    // If going into a QUANTITY or used for eth-context representation, don't pad
    let bigintToHex num =
        num |> bigintToHexList |> intListToString

    // If conversion will be used in an eth DATA value, it must be two chars
    // per byte. Necessary if this hex value will also be converted back to
    // a bigint, as length is used to imply the sign.
    let bigintToHexPadded num =
        let res = num |> bigintToHexList |> intListToString

        if (res.Length % 2 = 0) then
            res
        else
            "0" + res

    let hexToBigInt hexString =
        bigint.Parse(hexString, NumberStyles.AllowHexSpecifier)

    // convenience partial application
    let strip0xAndConvertToBigInt = strip0x >> hexToBigInt


    ///
    /// Wei/ETH conversion
    ///


    // Returns a string representation because most use-cases for dealing in
    // 'ETH' are on the human side of an interface.
    let convertWeiToEth wei =
        let (eth, frac) = bigint.DivRem(wei, weiDiv)
        let rem = frac.ToString().PadLeft(18, '0')
        $"{eth.ToString()}.{rem.Remove(17)}"

    // Much like the above, returns a bigint for working in wei on the RPC-side of the interface.
    let convertEthToWei (eth: string) =
        let _eth =
            match not (eth.Contains('.')) with
            | true -> eth + "."
            | false -> eth

        let xa = _eth.Split('.')
        let (x, xs) = (xa.[0], xa.[1])
        let e = x.TrimStart('0')
        let wei = xs.PadRight(18, '0').Remove(18)
        bigint.Parse(e + wei)


///
/// RPC method module
///


module RPCMethodFunctions =
    open Types


    ///
    /// lifters
    ///


    let wrapEthMethod m = m |> EthMethod
    let wrapShhMethod m = m |> ShhMethod
    let wrapNetMethod m = m |> NetMethod
    let wrapWeb3Method m = m |> Web3Method


    ///
    /// Convert calltype into json string representation
    ///


    let bindEthMethod (m: EthMethod) =
        match m with
        | EthMethod.Accounts -> "eth_accounts"
        | EthMethod.BlockNumber -> "eth_blockNumber"
        | EthMethod.Call -> "eth_call"
        | EthMethod.Coinbase -> "eth_coinbase"
        | EthMethod.CompileLLL -> "eth_compileLLL"
        | EthMethod.CompileSerpent -> "eth_compileSerpent"
        | EthMethod.CompileSolidity -> "eth_compileSolidity"
        | EthMethod.EstimateGas -> "eth_estimasGas"
        | EthMethod.GasPrice -> "eth_gasPrice"
        | EthMethod.GetBalance -> "eth_getBalance"
        | EthMethod.GetBlockByHash -> "eth_getBlockByHash"
        | EthMethod.GetBlockbyNumber -> "eth_getBlockbyNumber"
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


///
/// RPC Parameter module
///


module RPCParamFunctions =
    open Types
    open Helpers


    ///
    /// lifters
    ///


    let wrapEthParams p = p |> EthParam
    let wrapNetParams p = p |> NetParam
    let wrapWeb3Params p = p |> Web3Param


    ///
    /// Convert call params into json string representation. Some params
    /// will apparently be deprecated soon, if not already. Log types
    /// not yet implemented.
    ///


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
