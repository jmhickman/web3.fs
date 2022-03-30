namespace web3.fs

open web3.fs.Types

module ContractFunctions =
    open System
    open FSharp.Data
    open SHA3Core.Keccak

    open Helpers
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Unwrappers/binders
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    ///
    /// Returns a function selector given a configured Keccak instance and the
    /// canonical representation of a function.
    /// 
    let returnFunctionSelector (digest: Keccak) (rep: CanonicalRepresentation) =
        match rep with
        | CanonicalFunctionRepresentation r ->
            digest.Hash(r).Remove(8)
            |> prepend0x
            |> EVMFunctionHash
        | CanonicalEventRepresentation r -> 
            digest.Hash(r) 
            |> prepend0x 
            |> EVMEventSelector
        | CanonicalErrorRepresentation r ->
            digest.Hash(r).Remove(8)
            |> prepend0x
            |> EVMFunctionHash
    
    
    ///
    /// Returns upwrapped EVMSelector value for use in transaction object construction.
    let bindEVMSelector a =
        match a with
        | EVMFunctionHash s -> s 
        | EVMEventSelector s -> s 
    
    
    ///
    /// Returns unwrapped canonical representation of a function, event or error.
    let bindCanonicalRepresentation a =
        match a with
        | CanonicalFunctionRepresentation s -> s
        | CanonicalEventRepresentation s -> s
        | CanonicalErrorRepresentation s -> s
        
        
    ///
    /// Returns upwrapped EVMFunctionInput string.
    let bindEVMFunctionInputs = function EVMFunctionInputs s -> s
    
    
    ///
    /// Returns upwrapped EVMFunctionOutput string.
    let bindEVMFunctionOutputs = function EVMFunctionOutputs s -> s
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // JsonValue functions for testing/extracting properties
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Returns the text of the "type" property.
    let getInnerTypeText (jVal: JsonValue) = jVal.GetProperty("type").InnerText()

    
    ///
    /// Predicate for filters
    let testPropertyInnerText (s: string) (jVal: JsonValue) =
        jVal.GetProperty("type").InnerText() = s

    
    ///
    /// Checks if the JsonValue 'type' value is tuple. Tupled values are treated differently in the logic.
    let checkForTuple (jVal: JsonValue) = (getInnerTypeText jVal).StartsWith("tuple") 

    
    let checkForTupleArray (jVal: JsonValue) =
        matchEVMInput "tuple\[\]" (getInnerTypeText jVal)
        
    
    let checkForSzTupleArray (jVal: JsonValue) =
        matchEVMInputSz "tuple\[([0-9]{1,2})\]" (getInnerTypeText jVal)
    
    
    ///
    /// Tupled values in the ABI can be 'tuple' 'tuple[]' or 'tuple[k]'. Grab the glyphs if present for appending to the end
    /// of the joined strings later.
    ///
    let extractEnder (jVal: JsonValue) =
        jVal.GetProperty("type").InnerText().Substring(5)


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for functions and returns
    /// an Option for the name, inputs, outputs and the state mutability parameter.
    ///
    let tryGetFunctionProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "function")
        |> Array.map (fun i ->
            (i.TryGetProperty("name"),
             i.TryGetProperty("inputs"),
             i.TryGetProperty("outputs"),
             i.TryGetProperty("statemutability")))


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for events and returns
    /// an option for the name, inputs, and the anonymous parameter.
    ///
    let tryGetEventProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "event")
        |> Array.map (fun i -> (i.TryGetProperty("name"), i.TryGetProperty("inputs"), i.TryGetProperty("anonymous")))


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for errors and returns
    /// an Option for the name and inputs.
    ///
    let tryGetErrorProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "error") // Just functions now
        |> Array.map (fun i -> (i.TryGetProperty("name"), i.TryGetProperty("inputs")))


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for the constructor and
    /// returns its inputs. Constructors never have a name or outputs. Only useful for deploying a contract
    /// programmatically.
    ///
    let tryGetConstructorProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "constructor")
        |> Array.map (fun i -> (i.TryGetProperty("inputs")))


    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'receive' function. Receive functions have no inputs, must be 'payable', and may not have outputs.
    /// Thus, this function only returns the presence of a receive function only.
    ///
    let tryGetReceive (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "receive")
        |> Array.tryHead


    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'fallback' function. The fallback may only have an empty argument tuple, or a bytes argument
    /// that the EVM will fill with the calldata of the txn that hit the fallback. It may only return a
    /// bytes as output. It may be payable.
    ///
    let tryGetFallback (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "fallback")
        |> Array.tryHead

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Contract function, event and error extraction
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Recursively send tupled components through the function in order to extract and format nested values properly.
    /// Non-tupled values are concatenated directly. ()'s inserted as needed.
    ///
    let rec collapseTuples (_input: JsonValue) =
        match _input with
        | JsonValue.Array elements ->
            elements
            |> Array.map (fun e ->
                if checkForTuple e then
                    $"""{collapseTuples (e.GetProperty("components"))}{extractEnder e}"""
                else
                    $"""{getInnerTypeText e}""")
            |> fun s -> String.Join(',', s)
            |> fun s -> $"({s})"
        | _ -> ""

    ///
    /// Returns an EVMDatatype based on a static lookup of the EVM datatype culled from the ABI. Some definite liberties
    /// are taken with regards to the exact precision of number types and bytes, arrays, etc. They are typically
    /// 'upcast' to the largest representation. Multi-dimensional arrays are not supported.
    /// 
    let typeLookup evmTypeString =
      match evmTypeString with
      | x when matchEVMInput "^address$" x -> Address ""
      | x when matchEVMInput "^address\[\]$" x -> AddressArray []
      | x when matchEVMInput "^address\[([0-9]{1,2})\]$" x  ->
          let count = matchEVMInputSz "^address\[([0-9]{1,2})\]$" x
          AddressArraySz (List.init count (fun _ -> ""))
      | x when matchEVMInput "^uint[0-9]{1,3}$" x -> Uint256 ""
      | x when matchEVMInput "^uint[0-9]{1,3}\[\]$" x -> Uint256Array []
      | x when matchEVMInput "^uint[0-9]{1,3}\[([0-9]{1,2})\]$" x  ->
          let count = matchEVMInputSz "uint[0-9]{1,3}\[([0-9]{1,2})\]$" x
          Uint256ArraySz (List.init count (fun _ -> ""))
      | x when matchEVMInput "^int[0-9]{1,3}$" x -> Int256 ""
      | x when matchEVMInput "^int[0-9]{1,3}\[\]$" x -> Int256Array []
      | x when matchEVMInput "^int[0-9]{1,3}\[([0-9]{1,2})\]$" x  ->
          let count = matchEVMInputSz "int[0-9]{1,3}\[([0-9]{1,2})\]$" x
          Int256ArraySz (List.init count (fun _ -> ""))
      | x when matchEVMInput "^bool" x -> Bool true
      | x when matchEVMInput "^bool\[\]$" x -> BoolArray []
      | x when matchEVMInput "^bool\[([0-9]{1,2})\]$" x  ->
          let count = matchEVMInputSz "^bool\[([0-9]{1,2})\]$" x
          BoolArraySz (List.init count (fun _ -> true))
      | x when matchEVMInput "^bytes$" x -> Bytes ""
      | x when matchEVMInput "^bytes[0-9]{1,3}$" x -> BytesSz ""
      | x when matchEVMInput "^bytes[0-9]{1,3}\[([0-9]{1,2})\]$" x -> BytesArraySz []
      | x when matchEVMInput "^bytes[0-9]{1,3}\[([0-9]{1,2})\]$" x  ->
          let count = matchEVMInputSz "bytes[0-9]{1,3}\[([0-9]{1,2})\]$" x
          BytesSzArraySz (List.init count (fun _ -> ""))
      | x when matchEVMInput "^string$" x -> EVMDatatype.String ""
      | x when matchEVMInput "^function$" x -> EVMDatatype.Function ""
      | x when matchEVMInput "^function\[([0-9]{1,2})\]$" x -> FunctionArraySz []
      | x when matchEVMInput "^function\[([0-9]{1,2})\]$" x  ->
          let count = matchEVMInputSz "function\[([0-9]{1,2})\]$" x
          FunctionArraySz (List.init count (fun _ -> ""))
      | _ -> Blob ""
    
    
    ///
    /// Returns an internal complete representation of a Solidity function's types, with some caveats. Types with
    /// variable precision are upcast to the largest value type (i.e., uint8 -> uint256) and multi-dimensional
    /// arrays are not supported.
    ///  
    let rec returnEVMTypes (_input: JsonValue) (acc: EVMDatatype list) =
        match _input with
        | JsonValue.Array elements ->
            elements
            |> Array.toList
            |> List.collect (fun e ->
                match e with
                | x when matchEVMInput "^tuple$" (getInnerTypeText x) ->
                    [EVMDatatype.Tuple (returnEVMTypes (x.GetProperty("components")) acc)] @ acc
                | x when matchEVMInput "^tuple\[\]$" (getInnerTypeText x) ->
                    [EVMDatatype.TupleArray (returnEVMTypes (x.GetProperty("components")) acc)] @ acc
                | x when matchEVMInput "^tuple\[([0-9]{1,2})\]$" (getInnerTypeText x) ->
                    let count = matchEVMInputSz "^tuple\[([0-9]{1,2})\]$" (getInnerTypeText x)
                    [EVMDatatype.TupleArraySz
                         (List.init count (fun _ ->
                            EVMDatatype.Tuple (returnEVMTypes (x.GetProperty("components")) acc)))] @ acc
                //| x when matchEVMInput "^bytes\[\]$" (getInnerTypeText x) ->
                //    [EVMDatatype.BytesArray (returnEVMTypes (x.GetProperty("components")) acc)] @ acc
                | x when matchEVMInput "^bytes\[([0-9]{1,2})\]$" (getInnerTypeText x)  ->
                    let count = matchEVMInputSz "bytes\[([0-9]{1,2})\]$" (getInnerTypeText x)
                    [EVMDatatype.BytesArraySz (List.init count (fun _ -> EVMDatatype.Bytes "" ))] @ acc
                //| x when matchEVMInput "^string\[\]$" (getInnerTypeText x) ->
                //    [EVMDatatype.StringArray (returnEVMTypes (x.GetProperty("components")) acc)] @ acc
                | x when matchEVMInput "^string\[([0-9]{1,2})\]$" (getInnerTypeText x)  ->
                    let count = matchEVMInputSz "string\[([0-9]{1,2})\]$" (getInnerTypeText x)
                    [EVMDatatype.StringArraySz (List.init count (fun _ -> EVMDatatype.String "" ))] @ acc
                | x -> 
                    getInnerTypeText x |> typeLookup |> fun r ->  [r] @ acc 
                    )
            
        | _ -> []
        
    ///
    /// Returns the state mutability parameter of the EVM function, given a JsonValue option.
    /// Defaults to 'nonpayable' as is the spec.
    ///
    let returnStateMutability (b: JsonValue option) =
        match b with
        | Some _b ->
            match _b.InnerText() with
            | "pure" -> Pure
            | "view" -> View
            | "payable" -> Payable
            | _ -> Nonpayable
        | None -> Nonpayable


    ///
    /// Returns the function name of an EVM function, given a JsonValue option. Defaults to an
    /// empty string, which may be non-compliant.
    ///
    let returnFunctionName (b: JsonValue option) =
        match b with
        | Some f -> f.InnerText()
        | None -> ""


    ///
    /// Returns the inputs of an EVM function (collapsing tuples along the way) given a JsonValue
    /// option. Defaults to the string "()".
    ///
    let returnInputs (b: JsonValue option) =
        match b with
        | Some jVal -> $"""{collapseTuples jVal}"""
        | None -> $"()"

    let returnOutputs (b: JsonValue option) =
       match b with
       | Some jVal -> returnEVMTypes jVal []
       | None -> []
       
       
    ///
    /// Returns the anonymous boolean of an EVM event, given a JsonValue option. Defaults to false.
    let returnAnonymous (b: JsonValue option) =
        match b with
        | Some jVal ->
            jVal.AsBoolean()
        | None -> false


    ///
    /// When supplied with an IntermediateFunctionRepresentation, returns the corresponding EVMFunction record.
    let returnEVMFunction (digest: Keccak) (interFunc: IntermediateFunctionRepresentation) =

        let _funcName, _inputs, _outputs, _stateMut = interFunc

        let name = returnFunctionName _funcName
        let inputs = returnInputs _inputs
        
        let hash =
            ($"{name}{inputs}"
             |> CanonicalFunctionRepresentation)
            |> returnFunctionSelector digest

        { name = name
          hash = hash
          inputs = inputs |> EVMFunctionInputs
          outputs = returnOutputs _outputs 
          config = returnStateMutability _stateMut }


    ///
    /// When supplied with an IntermediateEventRepresentation, returns the corresponding EVMEvent record.
    let returnEVMEvent (digest: Keccak) (interEvent: IntermediateEventRepresentation) =

        let _eventName, _inputs, _anon = interEvent

        let name = returnFunctionName _eventName
        let anon = returnAnonymous _anon
        let inputs = returnInputs _inputs

        let hash =
            ($"{name}{inputs}" |> CanonicalEventRepresentation)
            |> returnFunctionSelector digest

        { name = name
          anonymous = anon
          inputs = inputs |> EVMFunctionInputs
          hash = hash }


    ///
    /// When supplied with an IntermediateErrorRepresentation, returns the corresponding EVMError record.
    let returnEVMError (digest: Keccak) (interError: IntermediateErrorRepresentation) =

        let _errName, _inputs = interError

        let name = returnFunctionName _errName
        let inputs = returnInputs _inputs

        let hash =
            ($"{name}{inputs}" |> CanonicalErrorRepresentation)
            |> returnFunctionSelector digest

        { name = name
          inputs = inputs |> EVMFunctionInputs
          hash = hash }


    ///
    /// When supplied with a Solidity contract ABI in Json format, returns a list of all functions as
    /// EVMFunctions. A Keccak hash digest is required to generate function selectors.
    ///
    let parseABIForFunctions (digest: Keccak) (json: JsonValue array) =
        json
        |> tryGetFunctionProperties
        |> Array.map (returnEVMFunction digest)
        |> Array.toList


    ///
    /// When supplied with a Solidity contract ABI in Json format, returns a list of EVMEvents.
    /// A Keccak hash digest is required to generate event selectors.
    ///
    let parseABIForEvents (digest: Keccak) (json: JsonValue array) =
        json
        |> tryGetEventProperties
        |> Array.map (returnEVMEvent digest)
        |> Array.toList


    ///
    /// When supplied with a Solidity contract ABI in Json format, returns a tuple of the constructor,
    /// the presence of a fallback function, and receive function if found.
    ///
    let parseABIForConstructorFallbackReceive (digest: Keccak) (json: JsonValue array) =

        let constructor =
            match tryGetConstructorProperties json with
            | [| Some r |] ->
                let x = $"constructor{collapseTuples r}"
                (x, digest.Hash(x))
            | _ -> ("constructor()", "0x90fa17bb")

        let fallback = tryGetFallback json |> returnInputs
        let receive = tryGetReceive json |> returnInputs
        (constructor, fallback, receive)

    ///
    /// When supplied with a Solidity contract ABI in Json format, returns a list of EVMErrors.
    /// A Keccak hash digest is required to generate function selectors.
    ///
    let parseABIForErrors (digest: Keccak) (json: JsonValue array) =
        json
        |> tryGetErrorProperties
        |> Array.map (returnEVMError digest)
        |> Array.toList


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Contract representation functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Returns a Result containing either a DeployedContract for interaction, or an error indicating
    /// a failure of the JsonValue parser to yield a top-level representation of the ABI. `chainId` is in hex notation,
    /// such as '0x01' (mainnet) or '0x04' (rinkeby). Can be partially applied if many contracts will be loaded from a
    /// map() of addresses, network and ABIs.
    ///
    let loadDeployedContract digest address chainId abi : LoadContractResult =
        let (ABI _abi) = abi

        match JsonValue.TryParse(_abi) with
        | Some json ->
            let _j = json.AsArray()
            let _fList = parseABIForFunctions digest _j
            let _eventList = parseABIForEvents digest _j
            let _errList = parseABIForErrors digest _j
            let _, fallback, receive = parseABIForConstructorFallbackReceive digest _j

            { address = address
              abi = abi
              functions = _fList
              events = _eventList
              errors = _errList
              fallback = fallback
              receive = receive
              chainId = chainId }
            |> Ok
        | None ->
            ContractParseFailure "Json was incorrectly formatted or otherwise failed to parse"
            |> Error

    
    ///
    /// Returns an UndeployedContract for use in `deployEthContract`. 
    let prepareUndeployedContract digest bytecode constructorArguments chainId abi =
        let (ABI _abi) = abi
        
        match JsonValue.TryParse(_abi) with
        | Some json ->
            let _j = json.AsArray()
            let (_, hash), _, _ = parseABIForConstructorFallbackReceive digest _j
            {
                abi = abi
                constructor = (hash |> EVMFunctionHash)
                bytecode = bytecode
                chainId = chainId
                constructorArguments = constructorArguments
            }
            |> Ok
        | None -> ContractParseFailure "Json was incorrectly formatted or otherwise failed to parse" |> Error
        
    
    //let convertToDeployedContract (newContract: UndeployedContract) (address: EthAddress) : DeployedContract =
    
  