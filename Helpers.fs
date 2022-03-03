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

    ///
    /// Verifies the correctness of the Transaction type of a call.
    let validateTxnType t =
        let reg = new Regex("^0x([0-9,a-f,A-F]){1,2}$")

        match reg.Match(t).Success with
        | true -> Some t
        | false -> None


    ///
    /// Verifies the correctness of QUANTITY data in a call.
    let validateQuantity s =
        let reg = new Regex("^0x([1-9a-f]+[0-9a-f]*|0)$")

        match reg.Match(s).Success with
        | true -> Some s
        | false -> None

    ///
    /// Verifies the correctness of DATA data in a call.
    let validateData s =
        let reg = new Regex("^0x([0-9a-f]{2})*$")

        match reg.Match(s).Success with
        | true -> Some s
        | false -> None

    ///
    /// Verifies the correctness of ADDRESS data in a call.
    let validateAddress s =
        let reg = new Regex("^0x[0-9,a-f,A-F]{40}$")

        match reg.Match(s).Success with
        | true -> Some s
        | false -> None

    ///
    /// When supplied with an UnvalidatedEthParam1559Call, returns a Result. The purpose
    /// is to ensure that the data supplied in each field is consistent with requirements
    /// for each type. Note that the resulting EthParam1559Call is not guaranteed to
    /// succeed or be syntactically valid EVM bytecode.
    ///
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

    /// Serializes a 1559 call to Json for sending in an RPC message.
    let createJsonObj (ethParams: EthParam1559Call) = Json.serializeEx jsonConfig ethParams

    ///
    /// Creates a parameter list for RPC calls that take such a flat list format.
    let concatParamString (list: string list) =
        list
        |> List.fold (fun acc s -> $"""{acc}"{s}", """) ""
        |> fun s -> s.TrimEnd(',', ' ')


    //
    // Hex and bigint functions
    //

    ///
    /// Prepends a hexadecimal specifier to a string.
    let prepend0x s = "0x" + s


    ///
    /// Removes a hexadecimal specifier from a string.
    let strip0x (s: string) =
        if s.StartsWith("0x") then
            s.Remove(0, 2)
        else
            s


    ///
    /// Based on blessed code found at https://stu.dev/bigint-to-string-in-any-base-fsharp/
    let bigintToIntList _base input =
        let rec loop (_base: int) input digits =
            let (quotient, remainder) = bigint.DivRem(input, bigint _base)

            match quotient with
            | zero when zero = 0I -> int remainder :: digits
            | _ -> loop _base quotient (int remainder :: digits)

        loop _base input []


    /// Partial application for convenience to set hexadecimal processing.
    let bigintToHexList = bigintToIntList 16


    let intListToString input =
        input
        |> List.fold (fun acc (x: int) -> $"""{acc}{x.ToString("X").ToLower()}""") ""

    ///
    /// Returns a hexadecimal string with no padding. Useful for QUANTITY values in
    /// the ABI.
    let bigintToHex num =
        num |> bigintToHexList |> intListToString


    ///
    /// Returns a hexadecimal string prepended with a 0 if necessary to adhere to
    /// ABI two's compliment storage for DATA types.
    ///
    let bigintToHexPadded num =
        let res = num |> bigintToHexList |> intListToString

        if (res.Length % 2 = 0) then
            res
        else
            "0" + res


    ///
    /// Converts a hexadecimal string to a BigInt. ABI specifies two's compliment storage
    /// so mind what strings are passed in.
    let hexToBigInt hexString =
        bigint.Parse(hexString, NumberStyles.AllowHexSpecifier)


    ///
    /// A partial application for convenience, does what it says
    let strip0xAndConvertToBigInt = strip0x >> hexToBigInt


    //
    // Wei/ETH conversion
    //

    ///
    /// Returns a string representation of the conversion of wei to Eth.
    let convertWeiToEth wei =
        let (eth, frac) = bigint.DivRem(wei, weiDiv)
        let rem = frac.ToString().PadLeft(18, '0')
        $"{eth.ToString()}.{rem.Remove(17)}"

    ///
    /// Returns a bigint for working in wei.
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

    ///
    /// Returns a Keccak hasher properly configured for 256bit hashes
    let newKeccakDigest () = new Keccak(KeccakBitType.K256)

    let returnFunctionSelector (digest: Keccak) (rep: CanonicalRepresentation) =
        match rep with
        | CanonicalFunctionRepresentation r ->
            digest.Hash(r).Remove(8)
            |> prepend0x
            |> EVMFunctionHash
        | CanonicalEventRepresentation r -> digest.Hash(r) |> prepend0x |> EVMEventSelector
        | CanonicalErrorRepresentation r ->
            digest.Hash(r).Remove(8)
            |> prepend0x
            |> EVMFunctionHash