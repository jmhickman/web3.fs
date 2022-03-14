namespace web3.fs

open web3.fs.Types

module Helpers =
    open System
    open System.Text
    open System.Globalization
    open System.Text.RegularExpressions

    open FSharp.Json
    open SHA3Core.Enums
    open SHA3Core.Keccak


    ///
    /// Returns unwrapped value and handles the defaults for any `None` cases.
    let constantsBind (c: ContractConstants) =
        let txn =
            match c.transactionType with
            | Some t -> t
            | None -> ""

        let maxFeePerGas =
            match c.maxFeePerGas with
            | Some t -> t
            | None -> ""

        let maxPriorityFeePerGas =
            match c.maxPriorityFeePerGas with
            | Some t -> t
            | None -> ""

        let data =
            match c.data with
            | Some t -> t
            | None -> []

        (txn, maxFeePerGas, maxPriorityFeePerGas, data)

    let bindFunctionIndicator (s: FunctionIndicator) contract =
        match s with
        | IndicatedFunction f -> f
        | ByString s ->
            contract.functions
            |> List.find (fun p -> p.name = s)


    let bindDeployedContract r =
        match r with
        | Ok o -> [ o ]
        | Error _ ->
            printfn "Contract failed to import"
            []

    //
    // RPC Data validation
    //

    ///
    /// Returns a string option after evaluating an input with a regex provided
    /// to the function.
    ///
    let validateInputs (reg: string) t =
        let reg = Regex(reg)

        match reg.Match(t).Success with
        | true -> Some t
        | false -> None

    ///
    /// Verifies the correctness of the Transaction type of a call.
    let validateTxnType t =
        validateInputs "^0x([0-9,a-f,A-F]){1,2}$" t

    ///
    /// Verifies the correctness of QUANTITY data in a call.
    let validateQuantity s =
        validateInputs "^0x([1-9a-f]+[0-9a-f]*|0)$" s

    ///
    /// Verifies the correctness of DATA data in a call.
    let validateData s = validateInputs "^0x([0-9a-f]{2})*$" s

    ///
    /// Verifies the correctness of ADDRESS data in a call.
    let validateAddress s =
        validateInputs "^0x[0-9,a-f,A-F]{40}$" s


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
    // Parameter handlers
    //

    ///
    /// Sets Json output to be compliant with RPC expectations.
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
    /// Returns a hexadecimal string with no padding. Useful for QUANTITY values in
    /// the ABI.
    let bigintToHex num =
        num |> fun n -> bigint.Parse(n).ToString("X")


    ///
    /// Returns a hexadecimal string prepended with a 0 if necessary to adhere to
    /// ABI two's compliment storage for DATA types.
    ///
    let bigintToHexPadded num =
        let res =
            num |> fun n -> bigint.Parse(n).ToString("X")

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
    /// A composition for convenience, does what it says
    let strip0xAndConvertToBigInt = strip0x >> hexToBigInt


    //
    // Wei/ETH conversion
    //

    ///
    /// Returns a string representation of the conversion of wei to Eth.
    let convertWeiToEth wei =
        let eth, frac = bigint.DivRem(wei, weiDiv)
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
        let x, xs = (xa.[0], xa.[1])
        let e = x.TrimStart('0')
        let wei = xs.PadRight(18, '0').Remove(18)
        bigint.Parse(e + wei)


    ///
    /// Returns a Keccak hasher properly configured for 256bit hashes
    let newKeccakDigest = Keccak(KeccakBitType.K256)

    let formatToBytes (s: string) =
        Encoding.ASCII.GetBytes(s)
        |> Array.map (fun (x: byte) -> String.Format("{0:X2}", x))
        |> String.concat ""


    ///
    /// Convenience function that returns a ContractConstants that contains the address used for the session, along
    /// with other values ready for manipulation via the `with` statement for modifying records. If the RPC is a wallet,
    /// these defaults should work perfectly well. If the RPC is an actual Ethereum node, the gas values and transaction
    /// type should be changed as required.
    /// 
    let createDefaultConstants address =
        {
        address = address
        transactionType = None
        maxFeePerGas = None
        maxPriorityFeePerGas = None
        data = None
        blockHeight = Some LATEST
    }