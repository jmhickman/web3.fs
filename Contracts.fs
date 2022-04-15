namespace web3.fs

open web3.fs.Types

[<AutoOpen>]
module ContractFunctions =
    open System
    open FSharp.Data
    open SHA3Core.Keccak
    
    open ABIFunctions
    open Helpers
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Unwrappers/binders
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Returns a function selector given a configured Keccak instance and the
    /// canonical representation of a function.
    /// 
    let private returnFunctionSelector (digest: Keccak) (rep: CanonicalRepresentation) =
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
    let internal bindEVMSelector a =
        match a with
        | EVMFunctionHash s -> s 
        | EVMEventSelector s -> s 
    
    
    ///
    /// Returns unwrapped canonical representation of a function, event or error.
    let internal bindCanonicalRepresentation a =
        match a with
        | CanonicalFunctionRepresentation s -> s
        | CanonicalEventRepresentation s -> s
        | CanonicalErrorRepresentation s -> s
        
        
    ///
    /// Returns upwrapped EVMFunctionInput string.
    let internal bindEVMFunctionInputs = function EVMFunctionInputs s -> s
    
    
    ///
    /// Returns upwrapped EVMFunctionOutput string.
    let internal bindEVMFunctionOutputs = function EVMFunctionOutputs s -> s
    
    
    ///
    /// Convenience function for taking search parameters and typing them for use in `findFunction`
    let public wrapFunctionHash hashString =
        hashString |> EVMFunctionHash |> SearchFunctionHash
        
    
    ///
    /// Convenience function for taking search parameters and typing them for use in `findFunction`
    let public wrapFunctionInputs inputString =
        inputString |> EVMFunctionInputs |> SearchFunctionInputs
    
    
    ///
    /// Convenience function for taking search parameters and typing them for use in `findFunction`
    let public wrapFunctionMutability state =
        state |> SearchFunctionMutability
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // JsonValue functions for testing/extracting properties
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Returns the text of the "type" property.
    let private getInnerTypeText (jVal: JsonValue) = jVal.GetProperty("type").InnerText()

    
    ///
    /// Predicate for filters
    let private testPropertyInnerText (s: string) (jVal: JsonValue) =
        jVal.GetProperty("type").InnerText() = s

    
    ///
    /// Checks if the JsonValue 'type' value is a tuple. Tupled values are treated differently in the logic.
    let private checkForTuple (jVal: JsonValue) = (getInnerTypeText jVal).StartsWith("tuple") 

    
    ///
    /// Checks if the JsonValue 'type' value is a tuple array. Tupled values are treated differently in the logic.
    let private checkForTupleArray (jVal: JsonValue) =
        matchEVMInput "tuple\[\]" (getInnerTypeText jVal)
        
    
    ///
    /// Checks if the JsonValue 'type' value is a sized tuple array. Tupled values are treated differently in the logic.
    let private checkForSzTupleArray (jVal: JsonValue) =
        matchEVMInputSz "tuple\[([0-9]{1,2})\]" (getInnerTypeText jVal)
    
    
    ///
    /// Tupled values in the ABI can be 'tuple' 'tuple[]' or 'tuple[k]'. Grab the glyphs if present for appending to the end
    /// of the joined strings later.
    ///
    let private extractEnder (jVal: JsonValue) =
        jVal.GetProperty("type").InnerText().Substring(5)


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for functions and returns
    /// an Option for the name, inputs, outputs and the state mutability parameter.
    ///
    let private tryGetFunctionProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "function")
        |> Array.map (fun i ->
            (i.TryGetProperty("name"),
             i.TryGetProperty("inputs"),
             i.TryGetProperty("outputs"),
             i.TryGetProperty("stateMutability")))


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for events and returns
    /// an option for the name, inputs, and the anonymous parameter.
    ///
    let private tryGetEventProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "event")
        |> Array.map (fun i -> (i.TryGetProperty("name"), i.TryGetProperty("inputs"), i.TryGetProperty("anonymous")))


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for errors and returns
    /// an Option for the name and inputs.
    ///
    let private tryGetErrorProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "error") // Just functions now
        |> Array.map (fun i -> (i.TryGetProperty("name"), i.TryGetProperty("inputs")))


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for the constructor and
    /// returns its inputs. Constructors never have a name or outputs. Only useful for deploying a contract
    /// programmatically.
    ///
    let private tryGetConstructorProperties (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "constructor")
        |> Array.map (fun i -> (i.TryGetProperty("inputs")))


    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'receive' function. Receive functions have no inputs, must be 'payable', and may not have outputs.
    /// Thus, this function only returns the presence of a receive function only.
    ///
    let private tryGetReceive (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "receive")
        |> Array.tryHead


    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'fallback' function. The fallback may only have an empty argument tuple, or a bytes argument
    /// that the EVM will fill with the calldata of the txn that hit the fallback. It may only return a
    /// bytes as output. It may be payable.
    ///
    let private tryGetFallback (jVals: JsonValue array) =
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
    let rec private collapseTuples (_input: JsonValue) =
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
    /// are taken with regards to the precision of number types. They are 'upcast' to the largest representation.
    /// Multi-dimensional arrays are not supported.
    /// 
    let private typeLookup evmTypeString =
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
      | x when matchEVMInput "^bytes\[([0-9]{1,2})\]$" x -> 
          let count = matchEVMInputSz "bytes[0-9]{1,3}\[([0-9]{1,2})\]$" x
          BytesArraySz (List.init count (fun _ -> Bytes ""))
      | x when matchEVMInput "^bytes\[\]$" x -> BytesArray []
      | x when matchEVMInput "^bytes[0-9]{1,3}$" x -> BytesSz ""
      | x when matchEVMInput "^bytes[0-9]{1,3}\[([0-9]{1,2})\]$" x  ->
          let count = matchEVMInputSz "bytes[0-9]{1,3}\[([0-9]{1,2})\]$" x
          BytesSzArraySz (List.init count (fun _ -> ""))
      | x when matchEVMInput "bytes[0-9]{1,3}\[\]$" x -> BytesSzArray []
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
    let rec private returnEVMTypes (_input: JsonValue) (acc: EVMDatatype list) =
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
                | x when matchEVMInput "^bytes\[([0-9]{1,2})\]$" (getInnerTypeText x)  ->
                    let count = matchEVMInputSz "bytes\[([0-9]{1,2})\]$" (getInnerTypeText x)
                    [EVMDatatype.BytesArraySz (List.init count (fun _ -> EVMDatatype.Bytes "" ))] @ acc
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
    let private returnStateMutability (b: JsonValue option) =
        match b with
        | Some _b ->
            match _b.InnerText() with
            | "pure" -> Pure
            | "view" -> View
            | "payable" -> Payable
            | _ -> Nonpayable
        | None ->
            Nonpayable


    ///
    /// Returns the function name of an EVM function, given a JsonValue option. Defaults to an
    /// empty string, which may be non-compliant.
    ///
    let private returnFunctionName (b: JsonValue option) =
        match b with
        | Some f -> f.InnerText()
        | None -> ""


    ///
    /// Returns a canonical representation of function inputs. Defaults to an empty tuple "()"
    let private returnCanonicalInputs (b: JsonValue option) =
        match b with
        | Some jVal -> $"""{collapseTuples jVal}"""
        | None -> "()"

    
    ///
    /// Returns a canonical representation of function outputs. Defaults to an empty tuple "()"
    let private returnCanonicalOutputs (b: JsonValue option) =
        match b with
        | Some jVal -> $"""{collapseTuples jVal}"""
        | None -> "()"
    
    
    ///
    /// Returns the outputs of an EVM function in term of web3.fs' internal representation of these types. This is not
    /// the same as the canonical representation (i.e., `(address)`)
    /// 
    let private returnInternalOutputs (b: JsonValue option) =
       match b with
       | Some jVal -> returnEVMTypes jVal []
       | None -> []
       
       
    ///
    /// Returns the anonymous boolean of an EVM event. Defaults to false.
    let private returnAnonymous (b: JsonValue option) =
        match b with
        | Some jVal ->
            jVal.AsBoolean()
        | None -> false


    ///
    /// Returns an EVMFunction corresponding to the intermediate representation supplied.
    let private returnEVMFunction (digest: Keccak) (interFunc: IntermediateFunctionRepresentation) =

        let _funcName, _inputs, _outputs, _stateMut = interFunc

        let name = returnFunctionName _funcName
        let inputs = returnCanonicalInputs _inputs
        
        let hash =
            ($"{name}{inputs}"
             |> CanonicalFunctionRepresentation)
            |> returnFunctionSelector digest

        { name = name
          hash = hash
          canonicalInputs = inputs |> EVMFunctionInputs
          internalOutputs = returnInternalOutputs _outputs
          canonicalOutputs = returnCanonicalOutputs _outputs |> EVMFunctionOutputs
          config = returnStateMutability _stateMut }


    ///
    /// Returns an EVMEvent corresponding to the intermediate representation supplied.
    let private returnEVMEvent (digest: Keccak) (interEvent: IntermediateEventRepresentation) =

        let _eventName, _inputs, _anon = interEvent

        let name = returnFunctionName _eventName
        let anon = returnAnonymous _anon
        let inputs = returnCanonicalInputs _inputs

        let hash =
            ($"{name}{inputs}" |> CanonicalEventRepresentation)
            |> returnFunctionSelector digest

        { name = name
          anonymous = anon
          inputs = inputs |> EVMFunctionInputs
          hash = hash }


    ///
    /// Returns an EVMError corresponding to the intermediate representation supplied.
    let private returnEVMError (digest: Keccak) (interError: IntermediateErrorRepresentation) =

        let _errName, _inputs = interError

        let name = returnFunctionName _errName
        let inputs = returnCanonicalInputs _inputs

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
    let private parseABIForFunctions (digest: Keccak) (json: JsonValue array) =
        json
        |> tryGetFunctionProperties
        |> Array.map (returnEVMFunction digest)
        |> Array.toList


    ///
    /// When supplied with a Solidity contract ABI in Json format, returns a list of EVMEvents.
    /// A Keccak hash digest is required to generate event selectors.
    ///
    let private parseABIForEvents (digest: Keccak) (json: JsonValue array) =
        json
        |> tryGetEventProperties
        |> Array.map (returnEVMEvent digest)
        |> Array.toList


    ///
    /// When supplied with a Solidity contract ABI in Json format, returns a tuple of the constructor,
    /// an optional fallback function, and optional receive function.
    ///
    let private parseABIForConstructorFallbackReceive (digest: Keccak) (json: JsonValue array) =

        let constructor =
            match tryGetConstructorProperties json with
            | [| Some r |] ->
                let x = $"constructor{collapseTuples r}"
                (x, digest.Hash(x).Remove(8))
            | _ -> ("constructor()", "0x90fa17bb")

        let fallback = tryGetFallback json |> returnCanonicalInputs
        let receive = tryGetReceive json |> returnCanonicalInputs
        (constructor, fallback, receive)

    
    ///
    /// When supplied with a Solidity contract ABI in Json format, returns a list of EVMErrors.
    /// A Keccak hash digest is required to generate function selectors.
    ///
    let private parseABIForErrors (digest: Keccak) (json: JsonValue array) =
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
    let public loadDeployedContract digest address chainId abi =
        let (ABI _abi) = abi
                
        match address |> wrapEthAddress with
        | Ok address -> 
            match JsonValue.TryParse(_abi) with
            | Some json ->
            
                let _j = json.AsArray()
                let _fList = parseABIForFunctions digest _j
                let _eventList = parseABIForEvents digest _j
                let _errList = parseABIForErrors digest _j
                let _, fallback, _ = parseABIForConstructorFallbackReceive digest _j
                let receive = {name = "receive"; hash = ("0xa3e76c0f" |> EVMFunctionHash); canonicalInputs = ("()" |> EVMFunctionInputs); internalOutputs = []; canonicalOutputs = ("()" |> EVMFunctionOutputs); config = Payable}                
                
                { address = address 
                  abi = abi
                  functions = _fList @ [receive]
                  events = _eventList
                  errors = _errList
                  fallback = fallback
                  chainId = chainId }
                |> Ok
            | None ->
                ContractParseFailure "Json was incorrectly formatted or otherwise failed to parse"
                |> Error
        | Error e -> e |> Error

    
    ///
    /// Returns an UndeployedContract for use in `deployEthContract`. 
    let public prepareUndeployedContract digest bytecode (constructorArguments: EVMDatatype list option) chainId abi =
        let (ABI _abi) = abi
        
        match JsonValue.TryParse(_abi) with
        | Some json ->
            let _j = json.AsArray()
            let (_, hash), _, _ = parseABIForConstructorFallbackReceive digest _j
            if hash = "0x90fa17bb" && constructorArguments.IsSome then
               ConstructorArgumentsToEmptyConstructorError |> Error
            else if not(hash = "0x90fa17bb") && constructorArguments.IsNone then
               ConstructorArgumentsMissingError |> Error
            else
                { abi = abi
                  constructor = (hash |> EVMFunctionHash)
                  bytecode = bytecode
                  chainId = chainId
                  constructorArguments = constructorArguments }
                |> Ok
        | None -> ContractParseFailure "Json was incorrectly formatted or otherwise failed to parse" |> Error
