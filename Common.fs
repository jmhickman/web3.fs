namespace Web3.fs

[<AutoOpen>]
module Common =
    open System
    open System.IO
    open System.Text
    open System.Globalization
    open System.Text.RegularExpressions

    open FSharp.Data
    open FSharp.Json
    open SHA3Core.Enums
    open SHA3Core.Keccak
    
    
    ///
    /// Returns a Keccak hasher configured for 256bit hashes.
    let public newKeccakDigest = Keccak(KeccakBitType.K256)
    
    
    ///
    /// The Json RPC type provider returns results as strings wrapped in double
    /// quotes. This causes issue elsewhere, so this function is used to remove
    /// the extraneous quotes when the values must be used in code. `TrimStart`
    /// and `TrimEnd` are specifically used because of potential knock-on
    /// effects of overzealous quote stripping.
    /// 
    let internal trimParameter (p: string) =
        p.TrimStart('"')
        |> fun s -> s.TrimEnd('"')    
    
    
    ///
    /// Common function of changing a RPC Result into a string and trimming "
    /// characters
    /// 
    let internal stringAndTrim (r: RPCResponse.Result) =
        r.ToString() |> trimParameter
    
    
    ///
    /// Removes whitespace and linebreaks from the input text. Intended for
    /// importing the ABI from a file.
    /// 
    let internal stripNewlinesAndWhitespace (input: string) =
        input
        |> fun s -> s.Replace("\r\n", String.Empty)
        |> fun s -> s.Replace(" ", String.Empty)        
    
    
    ///
    /// Returns a string formatted into a hexadecimal representation of its
    /// UTF-8 bytes.
    /// 
    let internal formatToBytes (s: string) =
        Encoding.UTF8.GetBytes(s)
        |> Array.map (fun (x: byte) -> String.Format("{0:X2}", x))
        |> String.concat ""


    let public importBytecodeFromFile (filetype: FileType) (path: string) =
        let solc = Regex(""""bytecode": {.*?"object": ("[a-f0-9A-F]*")""", RegexOptions.Singleline)
        let foundry = Regex(""""bytecode": {.*?"object": ("0x[a-f0-9A-F]*")""", RegexOptions.Singleline)
        let remix = Regex("""object": ("[a-f0-9A-F]*")""", RegexOptions.Singleline)
        let other = Regex(""""bytecode": ("[a-f0-9A-F]*")""", RegexOptions.Singleline)
                
        use file = new StreamReader(path)
        let contents = file.ReadToEnd ()
        
        match filetype with
        | Solc -> solc.Match(contents).Groups[1].Value |> trimParameter |> RawContractBytecode
        | Foundry -> foundry.Match(contents).Groups[1].Value |> trimParameter |> fun s -> s.Remove(0, 2) |> RawContractBytecode
        | Remix -> remix.Match(contents).Groups[1].Value |> trimParameter |> RawContractBytecode
        | Other -> other.Match(contents).Groups[1].Value |> trimParameter |> RawContractBytecode
        

    ///
    /// Returns the ABI string from a file. This can be the .abi emitted by the
    /// solc compiler, or Remix output copied into a file and saved.
    /// 
    let public importABIFromFile (path:string) =
        use file = new StreamReader(path)
        file.ReadToEnd()
        |> stripNewlinesAndWhitespace
        |> ABI
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Hex and bigint functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    ///
    /// Prepends a hexadecimal specifier to a string if one is not already
    /// present.
    /// 
    let public prepend0x (s: string) =
        if not(s.StartsWith("0x")) then $"0x{s}"
        else s


    ///
    /// Removes a hexadecimal specifier from a string if one is present.
    let public strip0x (s: string) =
        if s.StartsWith("0x") then s.Remove(0, 2)
        else s


    ///
    /// Returns the checksummed version of a string as an EthAddress.
    let public returnChecksumAddress address =
        let _addr = address |> strip0x
        let digest = newKeccakDigest
        
        digest.Hash(_addr).Remove(40)
        |> String.mapi(fun iter char ->
            if Int32.Parse(char.ToString(), NumberStyles.HexNumber) > 7 then
                Char.ToUpper(_addr[iter])
            else _addr[iter] )
        |> prepend0x
        |> EthAddress
    
    
    ///
    /// Returns the byte array representation of a hex string
    let internal returnByteArray (input: string) =
        Convert.FromHexString(input)
    
    ///
    /// Returns a hexadecimal string with no leading 0's.
    let public bigintToHex num =
        if num = "0" then
            "0x0"
        else num |> fun n ->
            bigint.Parse(n).ToString("X").TrimStart('0').ToLower() |> prepend0x


    ///
    /// Converts a hexadecimal string to a BigInt. Use this when the string
    /// being supplied was originally a QUANTITY in EVM/RPC terms. BigInteger
    /// will emit negative numbers when there is no leading 0 and the sign is
    /// ambiguous.
    /// 
    let public hexToBigintUnsigned (hexString: string) =
        hexString |> strip0x |> fun s ->
            if not(s.StartsWith('0')) then
                bigint.Parse($"0{s}", NumberStyles.AllowHexSpecifier)
            else
                s |> trimParameter |> fun h -> bigint.Parse(h, NumberStyles.AllowHexSpecifier)
    
    ///
    /// Converts a hexadecimal string to a BigInt. Does not attempt to strip
    /// leading 0's, so that BigInteger.Parse() can correctly emit negative
    /// integers.
    /// 
    let internal hexToBigint hexString =
        hexString |> strip0x |> trimParameter |> fun h -> bigint.Parse(h, NumberStyles.AllowHexSpecifier)


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Wei/ETH conversion
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    ///
    /// Converts wei into larger denominations.
    let private convertFromWei divisor padding quantity =
        let eth, frac = bigint.DivRem(quantity, divisor)
        let rem = frac.ToString().PadLeft(padding, '0')
        $"{eth.ToString()}.{rem.Remove(padding)}"

    
    ///
    /// Takes larger denominations of Ethereum value and returns wei.
    let private convertToWei points (eth: string) =
        let _eth =
            match not (eth.Contains('.')) with
            | true -> eth + "."
            | false -> eth

        let xa = _eth.Split('.')
        let x, xs = (xa[0], xa[1])
        let e = x.TrimStart('0')
        let wei = xs.PadRight(points, '0').Remove(points)
        bigint.Parse(e + wei).ToString()
    
    
    ///
    /// Converts an `Ether` or `Gwei` into `Wei` terms. 
    /// Invalid small values of Ether or Gwei will simply return 0.
    /// 
    let public asWei quantity =
        match quantity with
        | GweiValue _gwei -> _gwei |> convertToWei 9
        | EtherValue _eth -> _eth |> convertToWei 18
        | _ -> "0"
        
        
    ///
    /// Converts an `Ether` into `Gwei` terms. This function is really
    /// only for presentation purposes. The EVM deals exclusively in `Wei`.
    /// 
    let rec public asGwei quantity =
        match quantity with
        | EtherValue _eth ->
            match _eth |> convertToWei 9 with
            | "0" -> asWei quantity |> fun result -> asGwei (WeiValue $"{result}")
            | x -> x
        | WeiValue _wei ->
            match bigint.Parse(_wei) |> convertFromWei gweiFactor 9 with
            | "0.000000000" -> "0"
            | x -> x
        | _ -> "0"
        
    
    ///
    /// Converts a `Gwei` or `Wei` into `Ether` terms. This function is really
    /// only for presentation purposes. The EVM deals exclusively in `Wei`.
    /// 
    let public asEth quantity =
        match quantity with
        | WeiValue _wei ->
            match bigint.Parse(_wei) |> convertFromWei weiFactor 18 with
            | "0.000000000000000000" -> "0"
            | x -> x
        | GweiValue _gwei ->
            match _gwei |> convertToWei 9 |> fun w -> bigint.Parse(w) |> convertFromWei weiFactor 18 with
            | "0.000000000000000000" -> "0"
            | x -> x.TrimEnd('0')
        | _ -> "Input was Ether"
    
    
    ///
    /// Function for presenting `Wei` quantities expressed as hexadecimal
    /// strings from the EVM. This does not actually check that the string is a
    /// hexadecimal string, beware.
    /// 
    let public hexAsWei quantity =
        quantity |> hexToBigintUnsigned |> fun i -> i.ToString()
        
        
    ///
    /// Function for presenting an 'Ether' quantity expressed from hexadecimal
    /// strings from the EVM. This does not actually check that the string is a
    /// hexadecimal string.
    let public hexAsEth quantity =
        quantity |> hexToBigintUnsigned |> fun i -> i.ToString() |> WeiValue |> asEth 
    
    
    ///
    /// An alias for `hexAsWei` that maintains the 'look' of the other `as_`
    /// functions. This does not actually check that the string is a
    /// hexadecimal string, beware.
    /// 
    let public asWeiHex quantity = hexAsWei quantity
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binders
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Intended to return either a singular EVMFunction, or error in the
    /// ambiguous or no-results cases.
    ///  
    let internal returnSingularResult (results: FunctionIndicator list) =
        match results.Length with
        | x when x = 1 -> results.Head |> Ok
        | x when x = 0 -> FunctionNotFoundError |> Error
        | _ -> results |> AmbiguousFunctionError |> Error
    
    
    ///
    /// Returns a single FunctionSelector.
    let internal findFunction contract search =
        match search with
        | ByName s ->
            contract.functions
            |> List.filter(fun p -> p.name = s)
            |> List.map (fun f -> f |> IndicatedFunction)
            |> returnSingularResult
        | ByNameAndHash nameAndHash ->
            let name, hash = nameAndHash
            contract.functions
            |> List.filter(fun p -> p.name = name)
            |> List.filter(fun p -> p.hash = hash)
            |> List.map (fun f -> f |> IndicatedFunction)
            |> returnSingularResult
        | ByNameAndInputs nameAndInputs ->
            let name, inputs = nameAndInputs
            contract.functions
            |> List.filter(fun p -> p.name = name)
            |> List.filter(fun p -> p.canonicalInputs = inputs)
            |> List.map (fun f -> f |> IndicatedFunction)
            |> returnSingularResult
        | ByNameAndOutputs nameAndOutputs ->
            let name, outputs = nameAndOutputs
            contract.functions
            |> List.filter(fun p -> p.name = name)
            |> List.filter(fun p -> p.canonicalOutputs = outputs)
            |> List.map (fun f -> f |> IndicatedFunction)
            |> returnSingularResult
        | ByNameAndMutability nameAndMutability ->
            let name, mutability = nameAndMutability
            contract.functions
            |> List.filter(fun p -> p.name = name)
            |> List.filter(fun p -> p.config = mutability)
            |> List.map (fun f -> f |> IndicatedFunction)
            |> returnSingularResult
        | Receive -> FunctionIndicator.Receive |> Ok
        | Fallback -> FunctionIndicator.Fallback |> Ok
        
    
    ///
    /// Used by the `contract__` functions to retrieve specific EVMFunctions
    /// for calls. Receive and Fallback are special cases to support directly
    /// calling these functions on contracts, and aren't typically used.
    /// Fallback also assumes Payable to keep implementation easy. At worst a
    /// warning is emitted on sending 0 value that can be ignored.
    ///  
    let rec internal bindFunctionIndicator s =
        match s with
        | IndicatedFunction f -> f
        | FunctionIndicator.Receive ->
            { name = "receive"
              hash = "0x" |> EVMFunctionHash
              canonicalInputs = "()" |> EVMFunctionInputs
              internalOutputs = []
              canonicalOutputs = "()" |> EVMFunctionOutputs
              config = Payable }
        | FunctionIndicator.Fallback ->
            { name = "fallback"
              hash = "0xd3adb33f" |> EVMFunctionHash
              canonicalInputs = "()" |> EVMFunctionInputs
              internalOutputs = []
              canonicalOutputs = "()" |> EVMFunctionOutputs
              config = Payable }

       
    ///
    /// Returns a list containing contracts whose import succeeded.  
    let public bindDeployedContract (result: Result<DeployedContract, Web3Error>) =
        match result with
        | Ok o -> [ o ]
        | Error _ -> []

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Data validation
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Function returns a string option when evaluating an input with the
    /// provided regex string.
    /// 
    let internal validateInputs (reg: string) t =
        let reg = Regex(reg)

        match reg.Match(t).Success with
        | true -> Some t
        | false -> None

    
    ///
    /// Verifies the correctness of the Transaction type of a call.
    let internal validateTxnType t =
        validateInputs "^0x([0-9a-fA-F]){1,2}$" t

    
    ///
    /// Verifies the correctness of QUANTITY data in a call.
    let internal validateQuantity s =
        validateInputs "^0x([1-9a-fA-F]+[0-9a-fA-F]*|0)$" s

    
    ///
    /// Verifies the correctness of DATA data in a call.
    let internal validateData s = validateInputs "^0x([0-9a-fA-F]{2})*$" s

    
    ///
    /// Verifies the correctness of ADDRESS data in a call.
    let internal validateAddress s =
        validateInputs "^0x[0-9a-fA-F]{40}$" s

    
    ///
    /// Generic function for checking an input string for a given match.
    let internal matchEVMInput (reg: string) t =
        let reg = Regex(reg)
        reg.Match(t).Success
        
    
    ///
    /// Special case for sized arrays of `matchEVMInput`.
    let internal matchEVMInputSz (reg: string) t =
        let reg = Regex(reg)
        let num = reg.Match(t).Groups.Item(1)
        int(num.Value)
        
        
    ///
    /// When supplied with an UnvalidatedEthParam1559Call, returns a Result.
    /// The purpose is to ensure that the data supplied in each field is
    /// consistent with requirements for each type. Note that the resulting
    /// EthParam1559Call is not guaranteed to succeed or be syntactically
    /// valid.
    ///
    let internal validateRPCParams (unvalidatedRpcParam: UnvalidatedEthParam1559Call) =
        match validateData unvalidatedRpcParam.udata with
        | Some data ->
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
              data = data }
            |> EthParam1559Call
            |> Ok
        | None ->
            $"Call/TXN object 'data' value is missing or not valid: {unvalidatedRpcParam.udata}" |> DataValidatorError
            |> Error


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Parameter handlers
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Sets Json output to be compliant with RPC expectations.
    let jsonConfig =
        JsonConfig.create (serializeNone = SerializeNone.Omit, unformatted = true)


    /// Serializes a 1559 call to Json for sending in an RPC message.
    let internal createJsonObj (ethParams: EthParam1559Call) = Json.serializeEx jsonConfig ethParams


    ///
    /// Wraps the C-ism try method in FSharp clothing
    let private tryGetBool (s: string) =
        let res, bool = Boolean.TryParse(s)
        match res with
        | true -> Some bool
        | false -> None
    
    
    ///
    /// Wraps the C-ism try method in FSharp clothing.
    let private tryGetFloat (s: string) =
        let res, flt = Single.TryParse(s)
        match res with
        | true -> Some flt
        | false -> None
     
    
    ///
    /// Makes calls to determine if the return value should be treated as a
    /// boolean or a float.
    /// 
    let private testValueForBoolFloat (s: string) =
        let tryB = tryGetBool s
        if tryB.IsNone then
            let tryF = tryGetFloat s
            if tryF.IsNone then
                $"\"{s}\""
            else $"{s}"
        else $"{s}"
    
    
    ///
    /// Creates a parameter list for RPC calls that take such a flat list format.
    let internal concatParamString (list: string list) =
        list
        |> List.fold (fun acc s ->
            let s' = testValueForBoolFloat s
            $"""{acc}{s'}, """) ""
        |> fun s -> s.TrimEnd(',', ' ')


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Miscellaneous
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///
    /// Unpacks Result from a RPCResponse.Root for logging. None case shouldn't
    /// be reached in normal circumstances.
    /// 
    let internal unpackRoot (r:RPCResponse.Root) =
        match r.Result with
        | Some root -> root
        | None -> RPCResponse.Result(JsonValue.Null)
        
        
    ///
    /// Returns the RPCResponse.Result as a simple string value.
    let private returnSimpleValue (result: RPCResponse.Result) =
        result |> stringAndTrim
    
    
    ///
    /// Returns a TransactionReceipt for use in `decomposeResult`.
    let private returnTransactionReceiptRecord (result: RPCResponse.Result) =
        let toAddr =
            let a = result.To.JsonValue.ToString() |> trimParameter
            if a = "null" then None else a |> returnChecksumAddress |> Some
            
        let contractAddress =
            let b = result.ContractAddress.JsonValue.ToString() |> trimParameter
            if b = "null" then None else b |> returnChecksumAddress |> Some
        
        { blockHash = result.BlockHash
          blockNumber = result.BlockNumber
          contractAddress = contractAddress
          cumulativeGasUsed = result.CumulativeGasUsed
          effectiveGasPrice = result.EffectiveGasPrice
          from = result.From |> returnChecksumAddress
          gasUsed = result.GasUsed
          logs = result.Logs |> Array.map (fun l -> l.JsonValue.ToString()) |> Array.toList
          logsBloom = result.LogsBloom
          status = result.Status
          toAddr = toAddr
          transactionHash = result.TransactionHash
          transactionIndex = result.TransactionIndex
          tType = result.Type }
           
    
    ///
    /// Returns a record of a mined transaction for use in `decomposeResult`. 
    let private returnMinedTransactionRecord (result: RPCResponse.Result) =
        let mined = RPCMinedTransaction.Parse(result.JsonValue.ToString()) 
        //let access = mined.AccessList |> Array.fold(fun acc i -> $"{acc}{i.ToString()}" ) ""
        let toAddress =
            match mined.To with
            | "" -> None
            | x ->  x |> returnChecksumAddress |> Some
        
        { MinedTransaction.blockHash = mined.BlockHash
          MinedTransaction.blockNumber = mined.BlockNumber
          MinedTransaction.from = mined.From |> returnChecksumAddress
          MinedTransaction.gas = mined.Gas
          MinedTransaction.gasPrice = mined.GasPrice
          MinedTransaction.hash = mined.Hash |> EthTransactionHash
          MinedTransaction.input = mined.Input
          MinedTransaction.nonce = mined.Nonce
          MinedTransaction.r = mined.R
          MinedTransaction.s = mined.S
          MinedTransaction.v = mined.V
          MinedTransaction.toAddr = toAddress
          MinedTransaction.transactionIndex = mined.TransactionIndex
          MinedTransaction.tType = mined.Type
          MinedTransaction.value = mined.Value }
        
    
    ///
    /// Returns a record of an Ethereum block for use in `decomposeResult`.
    let private returnEthBlock (result: RPCResponse.Result) =
        let ethBlock = RPCBlock.Parse(result.JsonValue.ToString())
        let uncles = ethBlock.Uncles |> Array.fold(fun acc i -> $"{acc}{i.ToString()}" ) ""
        { baseFeePerGas = ethBlock.BaseFeePerGas
          difficulty = ethBlock.Difficulty
          extraData = ethBlock.ExtraData
          gasLimit = ethBlock.GasLimit
          gasUsed = ethBlock.GasUsed
          hash = ethBlock.Hash
          logsBloom = ethBlock.LogsBloom
          miner = ethBlock.Miner
          number = ethBlock.Number
          parentHash = ethBlock.ParentHash
          receiptsRoot = ethBlock.ReceiptsRoot
          sha3Uncles = ethBlock.Sha3Uncles
          size = ethBlock.Size
          stateRoot = ethBlock.StateRoot
          timestamp = ethBlock.Timestamp
          totalDifficulty = ethBlock.TotalDifficulty
          transactions = ethBlock.Transactions |> Array.toList
          transactionsRoot = ethBlock.TransactionsRoot
          uncles = [uncles] }
    
    
    ///
    /// Returns a decomposed RPC response record matching the output of the
    /// given EthMethod
    /// 
    let internal convertRPCResponseToCallResponses method result =
        result
        |> Result.bind (
            fun root ->
                let result = unpackRoot root
                match method with
                | EthMethod.GetBlockByHash
                | EthMethod.GetBlockByNumber -> returnEthBlock result |> Block |> Ok
                | EthMethod.GetTransactionByHash
                | EthMethod.GetTransactionByBlockHashAndIndex
                | EthMethod.GetTransactionByBlockNumberAndIndex ->
                    returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionReceipt ->
                    returnTransactionReceiptRecord result |> TransactionReceiptResult |> Ok
                | _ -> returnSimpleValue result |> SimpleValue |> Ok )
        

    ///
    /// Type for working with the output of Contract and RPC interactions
    /// (CallResponses).
    type Unwrap = class end
        with
        
        ///
        /// Returns an unwrapped EthBlock.
        static member block env result =
            match result |> env.emit with
            | Block ethBlock -> ethBlock
            | _ -> nullEthBlock
        
        ///
        /// Returns an unwrapped MinedTransaction.
        static member transaction env result =
            match result |> env.emit with
            | Transaction transaction -> transaction
            | _ -> nullMinedTransaction
        
        ///
        /// Returns a unwrapped TransactionReceipt. 
        static member transactionReceipt env result =
            match result |> env.emit with
            | TransactionReceiptResult rpcTransactionResponse -> rpcTransactionResponse
            | _ -> nullTransactionReceipt
            
        ///
        /// Returns an unwrapped string from an RPC call.
        static member rpcMethod env result =
            match result |> env.emit with
            | SimpleValue s -> s
            | _ -> "wrong unwrap or upstream web3 error"
        
        ///
        /// Returns an unwrapped list of EVMDatatypes from a contract call.
        static member call env result =
            match result |> env.emit with
            | CallResult evmDatatypes -> evmDatatypes 
            | _ -> [EVMDatatype.String "wrong unwrap or upstream web3 error"]
            
    ///
    /// This function returns the 256bit hash of the input ENS name string.
    /// The function also does this in a way that doesn't fit the documentation
    /// available for this process. That documentation says to apply a
    /// normalization process to the string, converting it into a PunyCode.
    /// When that process is used, names containing emoji, like
    /// `🦍frax-ape🦍.eth`, output the incorrect hash. Through a lot of
    /// experimentation, converting the strings into byte array before
    /// processing got the correct hashes to be emitted. So that's how it
    /// works.
    ///  
    let internal convertENSName (name: string) = 
        let digest = newKeccakDigest
        
        let labels =
            name.Split('.')
            |> Array.toList
            |> List.rev
            |> List.append [""]
            //|> fun p -> printfn $"{p}"; p
    
        let rec hashName (labels: string list) acc =
            match labels with
            | head::tail ->
                match head with
                | "" ->
                    hashName tail ENSZero
                | x ->
                    let acc =
                        x
                        |> UTF8Encoding.UTF8.GetBytes
                        |> digest.Hash
                        |> returnByteArray
                        |> Array.append acc
                        |> digest.Hash
                        |> returnByteArray
                    
                    hashName tail acc
            | [] ->
                Convert.ToHexString(acc).ToLower() |> prepend0x
        
        hashName labels [||]