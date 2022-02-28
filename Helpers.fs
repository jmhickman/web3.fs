namespace web3.fs

module Helpers =
    open System.Globalization
    open System.Text.RegularExpressions

    open Types
    open FSharp.Json
    open FSharp.Data
    open SHA3Core.Enums
    open SHA3Core.Keccak


    //
    // validators for the RPC data formats QUANTITY, DATA, TAG, ADDRESS
    //


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


    //
    // Parameter list handlers
    //


    let jsonConfig =
        JsonConfig.create (serializeNone = SerializeNone.Omit, unformatted = true)

    /// Create a serialized Json representation of call
    let createJsonObj (ethParams: EthParam1559Call) = Json.serializeEx jsonConfig ethParams

    /// For 'dumb' list-style parameter types.
    let concatParamString (list: string list) =
        list
        |> List.fold (fun acc s -> $"""{acc}"{s}", """) ""
        |> fun s -> s.TrimEnd(',', ' ')


    //
    // Hex and bigint functions
    //


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

    /// partial application for hexadecimal
    let bigintToHexList = bigintToIntList 16

    let intListToString input =
        input
        |> List.fold (fun acc (x: int) -> $"""{acc}{x.ToString("X").ToLower()}""") ""

    // If going into a QUANTITY or used for eth-context representation, don't pad
    let bigintToHex num =
        num |> bigintToHexList |> intListToString

    /// If conversion will be used in an eth DATA value, it must be two chars
    /// per byte. Necessary if this hex value will also be converted back to
    /// a bigint, as length is used to imply the sign.
    let bigintToHexPadded num =
        let res = num |> bigintToHexList |> intListToString

        if (res.Length % 2 = 0) then
            res
        else
            "0" + res

    let hexToBigInt hexString =
        bigint.Parse(hexString, NumberStyles.AllowHexSpecifier)

    /// A partial application for convenience, does what it says
    let strip0xAndConvertToBigInt = strip0x >> hexToBigInt


    //
    // Wei/ETH conversion
    //


    /// Returns a string representation because most use-cases for dealing in
    /// 'ETH' are on the human side of an interface.
    let convertWeiToEth wei =
        let (eth, frac) = bigint.DivRem(wei, weiDiv)
        let rem = frac.ToString().PadLeft(18, '0')
        $"{eth.ToString()}.{rem.Remove(17)}"

    /// Much like the above, returns a bigint for working in wei on the RPC-side of the interface.
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


    //
    // Contract helpers
    //


    let getInnerTypeText (jval: JsonValue) = jval.GetProperty("type").InnerText()

    /// Checks if the JsonValue 'type' value is tuple. Tupled values are treated differently in the logic.
    let checkForTuple (jval: JsonValue) =
        if (getInnerTypeText jval).StartsWith("tuple") then
            true
        else
            false

    /// Tupled values in the ABI can be 'tuple' 'tuple[]' or 'tuple[k]'. Grab the glyphs if present for appending to the end
    /// of the joined strings later.
    let extractEnder (jval: JsonValue) =
        jval.GetProperty("type").InnerText().Substring(5)

    let newKeccakDigest () = new Keccak(KeccakBitType.K256)

    let returnFunctionSelector (digest: Keccak) (canonicalFunction: CanonicalFunctionRepresentation) =
        let (CanonicalFunctionRepresentation rep) = canonicalFunction
        (prepend0x (digest.Hash(rep).Remove(8)))
