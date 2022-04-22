namespace web3.fs

open web3.fs.Types

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
    /// The Json RPC type provider returns results that have their own strings wrapped in double quotes. This
    /// causes issue elsewhere, so this function is used here and there to remove extraneous quotes when the values
    /// must be used in the code. `TrimStart` and `TrimEnd` are specifically used because of potential knock-on effects
    /// of overzealous quote stripping.
    /// 
    let internal trimParameter (p: string) =
        p
        |> fun s -> s.TrimStart('"')
        |> fun s -> s.TrimEnd('"')    
    
    
    ///
    /// Common function of changing a RPC Result into a string and trimming " characters
    let internal stringAndTrim (r: RPCResponse.Result) =
        r.ToString() |> trimParameter
    
    
    ///
    /// Returns a string formatted into a hexadecimal representation of its UTF-8 bytes.
    let internal formatToBytes (s: string) =
        Encoding.UTF8.GetBytes(s)
        |> Array.map (fun (x: byte) -> String.Format("{0:X2}", x))
        |> String.concat ""


    
    ///
    /// Returns the bytecode of a compiled contract from a json file, as in the output of the `solc` binary. Note, the
    /// path string should be triple quoted if you want to use Windows file path specifiers. Otherwise, use forward
    /// slashes, i.e. "c:/users/user/some/more/path/contract.json"
    /// 
    let public returnBytecodeFromFile (path: string) =
        let file = new StreamReader(path)
        let obj = ContractBytecode.Parse(file.ReadToEnd()) |> fun r -> r.Bytecode
        file.Dispose()
        obj |> RawContractBytecode
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Hex and bigint functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///
    /// Prepends a hexadecimal specifier to a string.
    let public prepend0x (s: string) =
        if not(s.StartsWith("0x")) then
            $"0x{s}"
        else s


    ///
    /// Removes a hexadecimal specifier from a string.
    let public strip0x (s: string) =
        if s.StartsWith("0x") then
            s.Remove(0, 2)
        else
            s

    ///
    /// Returns a hexadecimal string with no padding. Useful for QUANTITY values in the ABI.
    let public bigintToHex num =
        if num = "0" then "0x0" else num |> fun n -> bigint.Parse(n).ToString("X").TrimStart('0').ToLower()


    ///
    /// Converts a hexadecimal string to a BigInt. ABI specifies two's compliment storage
    /// so mind what strings are passed in. This attempts to negate issues with certain
    /// hex strings, and is a bodge.
    /// 
    let public hexToBigIntP (hexString: string) =
        if not(hexString.StartsWith('0')) then
            bigint.Parse($"0{hexString}", NumberStyles.AllowHexSpecifier)
        else
            hexString |> trimParameter |> fun h -> bigint.Parse(h, NumberStyles.AllowHexSpecifier)
    
    ///
    /// Converts a hexadecimal string to a BigInt. ABI specifies two's compliment storage
    /// so mind what strings are passed in.
    /// 
    let public hexToBigInt (hexString: string) =
        hexString |> strip0x |> trimParameter |> fun h -> bigint.Parse(h, NumberStyles.AllowHexSpecifier)

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Wei/ETH conversion
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///
    /// Returns a string quantity of Eth, given a BigInteger representation of wei. To convert wei from an RPC response,
    /// first run that string through hexToBigIntP.
    /// 
    let public convertWeiToEth wei =
        let eth, frac = bigint.DivRem(wei, weiDiv)
        let rem = frac.ToString().PadLeft(18, '0')
        $"{eth.ToString()}.{rem.Remove(17)}"

    
    ///
    /// Returns a string for working in wei. A string is chosen because the actual input used in the 'value' argument
    /// is a string, not a naked BigInt.
    let public convertEthToWei (eth: string) =
        let _eth =
            match not (eth.Contains('.')) with
            | true -> eth + "."
            | false -> eth

        let xa = _eth.Split('.')
        let x, xs = (xa.[0], xa.[1])
        let e = x.TrimStart('0')
        let wei = xs.PadRight(18, '0').Remove(18)
        bigint.Parse(e + wei).ToString()

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Binders
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Returns unwrapped value and handles the defaults for any `None` cases.
    let internal constantsBind (c: ContractConstants) =
        let txn =
            match c.transactionType with
            | Some t -> t
            | None -> "0x2"

        let maxFeePerGas =
            match c.maxFeePerGas with
            | Some t -> t |> bigintToHex |> prepend0x
            | None -> ""

        let maxPriorityFeePerGas =
            match c.maxPriorityFeePerGas with
            | Some t -> t |> bigintToHex |> prepend0x
            | None -> ""

        let data =
            match c.arguments with
            | Some t -> t
            | None -> []

        (txn, maxFeePerGas, maxPriorityFeePerGas, data)

        
    ///
    /// Returns a list of FunctionIndicators that matched the input FunctionSearchTerm criteria. These 
    /// FunctionIndicators can be used directly in `makeEth_` calls instead of using `ByString "someFunction"`. This is 
    /// also a way to find the right function if a contract uses overloads, by filtering for the outputs or inputs.
    /// 
    let public findFunction contract search =
        match search with
        | Name s ->
            contract.functions
            |> List.filter(fun p -> p.name = s)
            |> List.map (fun f -> f |> IndicatedFunction)
        | SearchFunctionHash evmSelector ->
            contract.functions
            |> List.filter(fun p -> p.hash = evmSelector)
            |> List.map (fun f -> f |> IndicatedFunction)
        | SearchFunctionInputs evmFunctionInputs ->
            contract.functions
            |> List.filter(fun p -> p.canonicalInputs = evmFunctionInputs)
            |> List.map (fun f -> f |> IndicatedFunction)
        | SearchFunctionOutputs evmFunctionOutputs ->
            contract.functions
            |> List.filter(fun p -> p.canonicalOutputs = evmFunctionOutputs)
            |> List.map (fun f -> f |> IndicatedFunction)
        | SearchFunctionMutability stateMutability ->
            contract.functions
            |> List.filter(fun p -> p.config = stateMutability)
            |> List.map (fun f -> f |> IndicatedFunction)
        
    
    ///
    /// Used by the `makeEth_` functions to handle users specifying functions by name (string) or by directly supplying
    /// the function. Receive and Fallback are special cases to support directly calling these functions against
    /// contracts, and aren't normally used. Fallback also assumes Payable to keep implementation easy, and at worst a
    /// warning is emitted on sending 0 value that can be ignored.
    ///  
    let rec internal bindFunctionIndicator contract s =
        match s with
        | IndicatedFunction f -> f
        | ByString s ->
            findFunction contract (s |> Name)
            |> List.head
            |> bindFunctionIndicator contract
        | Receive ->
            { name = "receive"
              hash = "0x" |> EVMFunctionHash
              canonicalInputs = "" |> EVMFunctionInputs
              internalOutputs = []
              canonicalOutputs = "" |> EVMFunctionOutputs
              config = Payable }
        | Fallback ->
            { name = "fallback"
              hash = "0xd3adb33f" |> EVMFunctionHash
              canonicalInputs = "" |> EVMFunctionInputs
              internalOutputs = []
              canonicalOutputs = "" |> EVMFunctionOutputs
              config = Payable }
                
            
                
            
    ///
    /// Returns a list containing contracts whose import succeeded.  
    let public bindDeployedContract result =
        match result with
        | Ok o -> [ o ]
        | Error _ -> []

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Data validation
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Returns a string option after evaluating an input with a regex provided to the function.
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
    /// Generic matcher for EVM types expressed in canonical form to internal types.
    let internal matchEVMInput (reg: string) t =
        let reg = Regex(reg)
        reg.Match(t).Success
        
    
    ///
    /// Special case for sized arrays of `matchEVMInput`
    let internal matchEVMInputSz (reg: string) t =
        let reg = Regex(reg)
        let num = reg.Match(t).Groups.Item(1)
        int(num.Value)
        
        
    ///
    /// When supplied with an UnvalidatedEthParam1559Call, returns a Result. The purpose
    /// is to ensure that the data supplied in each field is consistent with requirements
    /// for each type. Note that the resulting EthParam1559Call is not guaranteed to
    /// succeed or be syntactically valid EVM bytecode.
    ///
    let internal validateRPCParams (unvalidatedRpcParam: UnvalidatedEthParam1559Call) =
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
    ///
    let private tryGetFloat (s: string) =
        let res, flt = Single.TryParse(s)
        match res with
        | true -> Some flt
        | false -> None
     
    
    ///
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
    /// Returns a Keccak hasher properly configured for 256bit hashes.
    let public newKeccakDigest = Keccak(KeccakBitType.K256)

    
    
        ///
    /// Unpacks Result from a RPCResponse.Root for logging
    let internal unpackRoot (r:RPCResponse.Root) =
        match r.Result with
        | Some r' -> r'
        | None -> RPCResponse.Result(JsonValue.Null)
        
        
    ///
    /// Returns the current block, meaning the last block at was included in the chain.
    let private returnSimpleValue (result: RPCResponse.Result) =
        result |> stringAndTrim
    
    
    ///
    /// Creates a TransactionReceiptResult from an incoming RPCResponse.
    let private returnTransactionReceiptRecord (result: RPCResponse.Result) =
        let toAddr =
            let a = result.To.JsonValue.ToString() |> trimParameter
            if a = "null" then None else a  |> EthAddress |> Some
            
        let contractAddress =
            let b = result.ContractAddress.JsonValue.ToString() |> trimParameter
            if b = "null" then None else b |> EthAddress |> Some
        
        { blockHash = result.BlockHash
          blockNumber = result.BlockNumber
          contractAddress = contractAddress
          cumulativeGasUsed = result.CumulativeGasUsed
          effectiveGasPrice = result.EffectiveGasPrice
          from = result.From
          gasUsed = result.GasUsed
          logs = result.Logs |> Array.map (fun l -> l.JsonValue.ToString()) |> Array.toList
          logsBloom = result.LogsBloom
          status = result.Status
          toAddr = toAddr
          transactionHash = result.TransactionHash
          transactionIndex = result.TransactionIndex
          tType = result.Type }
           
    
    ///
    /// Returns a record of a mined transaction for use in decomposeResult 
    let private returnMinedTransactionRecord (result: RPCResponse.Result) =
        let mined = RPCMinedTransaction.Parse(result.JsonValue.ToString()) 
        let access = mined.AccessList |> Array.fold(fun acc i -> $"{acc}{i.ToString()}" ) ""
        
        { MinedTransaction.accessList = [access]
          MinedTransaction.blockHash = mined.BlockHash
          MinedTransaction.blockNumber = mined.BlockNumber
          MinedTransaction.chainId = mined.ChainId
          MinedTransaction.from = mined.From |> EthAddress
          MinedTransaction.gas = mined.Gas
          MinedTransaction.gasPrice = mined.GasPrice
          MinedTransaction.hash = mined.Hash |> EthTransactionHash
          MinedTransaction.input = mined.Input
          MinedTransaction.maxFeePerGas = mined.MaxFeePerGas
          MinedTransaction.maxPriorityFeePerGas = mined.MaxPriorityFeePerGas
          MinedTransaction.nonce = mined.Nonce
          MinedTransaction.r = mined.R
          MinedTransaction.s = mined.S
          MinedTransaction.v = mined.V
          MinedTransaction.toAddr = mined.To |> EthAddress
          MinedTransaction.transactionIndex = mined.TransactionIndex
          MinedTransaction.tType = mined.Type
          MinedTransaction.value = mined.Value }
        
    
    ///
    /// Returns a record of an Ethereum block for use in `decomposeResult` 
    let private returnEthBlock (result: RPCResponse.Result) =
        let ethBlock = RPCBlock.Parse(result.JsonValue.ToString())
        let uncles = ethBlock.Uncles |> Array.fold(fun acc i -> $"{acc}{i.ToString()}" ) ""
        { author = ethBlock.Author
          baseFeePerGas = ethBlock.BaseFeePerGas
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
          sealFields = ethBlock.SealFields |> Array.toList
          sha3Uncles = ethBlock.Sha3Uncles
          size = ethBlock.Size
          stateRoot = ethBlock.StateRoot
          timestamp = ethBlock.Timestamp
          totalDifficulty = ethBlock.TotalDifficulty
          transactions = ethBlock.Transactions |> Array.toList
          transactionsRoot = ethBlock.TransactionsRoot
          uncles = [uncles] }
    
    
    ///
    /// Returns a decomposed RPC response record matching the output of the given EthMethod
    let internal decomposeRPCResult method result =
        result
        |> Result.bind (
            fun root ->
                let result = unpackRoot root
                match method with
                | EthMethod.Accounts -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.BlockNumber -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.Coinbase -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.ChainId -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GasPrice -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetBalance -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetBlockByHash -> returnEthBlock result |> Block |> Ok
                | EthMethod.GetBlockByNumber -> returnEthBlock result |> Block |> Ok
                | EthMethod.GetBlockTransactionCountByHash -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetBlockTransactionCountByNumber -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetCode -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetStorageAt -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetTransactionCount -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetTransactionByHash -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionByBlockHashAndIndex -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionByBlockNumberAndIndex -> returnMinedTransactionRecord result |> Transaction |> Ok
                | EthMethod.GetTransactionReceipt -> returnTransactionReceiptRecord result |> TransactionReceiptResult |> Ok
                | EthMethod.GetUncleCountByBlockHash -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.GetUncleCountByBlockNumber -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.ProtocolVersion -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.Syncing -> returnSimpleValue result |> SimpleValue |> Ok
                | EthMethod.Sign -> returnSimpleValue result |> SimpleValue |> Ok
                | _ -> EthCallIntoNonCallPipeline |> Error
            )
        
    ///
    /// Unwraps CallResponses to a SimpleValue
    let public unwrapSimpleValue callResponse =
        match callResponse with
        | SimpleValue s -> s
        | _ -> "wrong unwrap or upstream web3 error"
    
    
    ///
    /// Unwraps CallResponses to a EVMDatatype list. Use with makeEthCall.
    let public unwrapCallResult callResponse =
        match callResponse with
        | CallResult evmDatatypes -> evmDatatypes
        | _ -> [EVMDatatype.String "wrong unwrap or upstream web3 error"]
    
    
    ///
    /// Unwraps CallResponses to a transaction receipt.
    let public unwrapTransactionReceipt callResponse =
        match callResponse with
        | TransactionReceiptResult rpcTransactionResponse -> rpcTransactionResponse
        | _ -> nullTransactionReceipt
    
    
    ///
    /// Unwraps CallResponses to a Transaction
    let public unwrapTransaction callResponse =
        match callResponse with
        | Transaction transaction -> transaction
        | _ -> nullMinedTransaction
        
    
    ///
    /// Unwraps CallResponse to a EthBlock
    let public unwrapBlock callResponse =
        match callResponse with
        | Block ethBlock -> ethBlock
        | _ -> nullEthBlock