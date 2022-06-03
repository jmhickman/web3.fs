namespace web3.fs

open Types

[<AutoOpen>]
module ContractFunctions =
    open System
    open FSharp.Data
    open SHA3Core.Keccak
    
    open Common
    open ABIFunctions
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Unwrappers/binders
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    ///
    /// Returns a function selector given a configured
    let private returnFunctionSelector (digest: Keccak) (rep: CanonicalRepresentation) =
        match rep with
        | CanonicalFunctionRepresentation r ->
            digest.Hash(r).Remove(8)
            |> prepend0x
            |> EVMFunctionHash
        | CanonicalEventRepresentation r -> 
            digest.Hash(r) 
            |> prepend0x 
            |> EVMEventHash
        | CanonicalErrorRepresentation r ->
            digest.Hash(r).Remove(8)
            |> prepend0x
            |> EVMFunctionHash


    ///
    ///Returns unwrapped canonical representation of a function, event or error.
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
        |> Array.map (fun i -> i.TryGetProperty("inputs"), i.TryGetProperty("stateMutability"))


    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'receive' function. Receive functions have no inputs, must be 'payable', and may not have outputs.
    /// Thus, this function only returns the presence of a receive function only.
    ///
    let private tryGetReceive (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "receive")
        |> Array.isEmpty
        |> not


    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'fallback' function. The fallback may only have an empty argument tuple, or a bytes argument
    /// that the EVM will fill with the calldata of the txn that hit the fallback. It may only return a
    /// bytes as output. It may be payable.
    ///
    let private tryGetFallback (jVals: JsonValue array) =
        jVals
        |> Array.filter (testPropertyInnerText "fallback")
        |> Array.isEmpty
        |> not

    
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
                    getInnerTypeText x |> typeLookup |> fun r -> [r] @ acc 
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
    let private parseABIForConstructor (digest: Keccak) (json: JsonValue array) =
        match tryGetConstructorProperties json with
        | [| Some inputs, stateMut |] ->
            let x = $"constructor{collapseTuples inputs}"
            digest.Hash(x).Remove(8), returnStateMutability stateMut
        | _ -> "90fa17bb", Nonpayable

    
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
    // Contract deployment and instance functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Basic check to ensure there is actually bytecode, in case the file used to provide it was malformed.
    let checkForBytecode (bytecode: RawContractBytecode) (abi: ABI) =
        let (RawContractBytecode s) = bytecode
        match s.Length with
        | x when x = 0 -> EmptyBytecodeError |> Error
        | _ -> abi |> Ok
    
    
    ///
    /// Generates Web3Error if the abi can't be parsed, implying interacting with the contract after deployment will
    /// fail. Otherwise, passes along the JsonValue.
    /// 
    let private canABIBeParsed (pipe: Result<ABI, Web3Error>) =
        pipe
        |> Result.bind( fun abi ->
            let (ABI _abi) = abi
            match JsonValue.TryParse(_abi) with
            | Some v -> v |> Ok
            | None -> ContractParseFailure "Json was incorrectly formatted or otherwise failed to parse" |> Error )
    
    
    ///
    /// Adapted version of canABIBeParsed for different pipeline
    let private pipeCanABIBeParsed (abi: ABI) (pipe: Result<EthAddress, Web3Error>) =
        pipe
        |> Result.bind(fun p -> 
            canABIBeParsed (abi |> Ok)// this is kinda stupid but whatever
            |> Result.bind(fun b ->
                (p, b) |> Ok ))
    
    
    ///
    /// Converts JsonValue to an array for further use.
    let private convertJsonValueToArray (pipe: Result<EthAddress * JsonValue, Web3Error>) =
        pipe |> Result.bind(fun (a, b) -> (a, b.AsArray()) |> Ok)
    
    
    ///
    /// Gets the components from the ABI
    let private getFunctionsEventsErrors digest (pipe: Result<EthAddress * JsonValue[], Web3Error>) =
        pipe
        |> Result.bind(fun (a, b) ->
            ( a,
              parseABIForFunctions digest b,
              parseABIForEvents digest b,
              parseABIForErrors digest b,
              tryGetFallback b,
              tryGetReceive b )
            |> Ok)
        
        
    ///
    /// Check that there aren't any function hash collisions in the contract ABI
    let private checkForHashCollisions digest (pipe: Result<JsonValue, Web3Error>) =
        pipe
        |> Result.bind(fun b ->
            let hashList = b.AsArray() |> parseABIForFunctions digest 
            hashList
            |> List.map( fun b' -> b'.hash)
            |> List.distinct
            |> fun l ->
                if not(l.Length = hashList.Length) then ContractABIContainsHashCollisionsError |> Error else b |> Ok ) 
       
    
    ///
    /// Extracts the constructor hash for this contract, so that checks can be made. Doesn't generate Web3Errors.
    let private buildConstructorHash digest (pipe: Result<JsonValue, Web3Error>) =
        pipe
        |> Result.bind(fun b ->
            b.AsArray()
            |> parseABIForConstructor digest
            |> fun (hash, stateMut) -> (hash |>trimParameter, stateMut) |> Ok)
    
    
    ///
    /// Generates a Web3Error if the user attempted to pass arguments to a `constructor()`
    let private constructorEmptyButArgsGiven (args: 'a list ) (pipe: Result<string * StateMutability, Web3Error>) =
        pipe
        |> Result.bind (fun (b, s ) ->
            if b = "90fa17bb" && args.Length > 0 then ConstructorArgumentsToEmptyConstructorError |> Error
            else (b, s ) |> Ok )
    
    
    ///
    /// Generates a Web3Error if the user failed to supply arguments to a constructor.
    let private constructorRequireArgsAndNoneWereGiven (args: 'a list ) (pipe: Result<string * StateMutability, Web3Error>) =
        pipe
        |> Result.bind (fun (b, s) ->
            if not(b = "90fa17bb") && args.IsEmpty then ConstructorArgumentsMissingError |> Error
            else (b, s ) |> Ok )
    
    
    ///
    /// Returns a Result containing either a DeployedContract for interaction, or a variety of errors such as the signer being
    /// on the wrong chain, malformed ABI, bad contract address, or errors in extracting components of the contract.
    /// 
    /// * `env`: a `Web3Environment`
    /// * `address`: A string indicating the address the deployed contract may be found out. For example, "0xd2BA82c4777a8d619144d32a2314ee620BC9E09c"
    /// * `chainId`: A string in hex notation indicating the network the contract is deployed to, such as '0x1' (mainnet) 
    /// or '0x4' (rinkeby). See `Types.fs` for a set of pre-defined network aliases.
    /// * `abi`: An `ABI` associated with the deployed contract.
    ///
    let public loadDeployedContract (abi: ABI) chainId address   =
        let digest = newKeccakDigest
        address
        |> wrapEthAddress
        |> pipeCanABIBeParsed abi
        |> convertJsonValueToArray
        |> getFunctionsEventsErrors digest
        |> Result.bind (fun (address, functions, events, errors, hasFallback, hasReceive) ->
            { address = address 
              abi = abi
              functions = functions
              events = events
              errors = errors
              hasFallback = hasFallback
              hasReceive = hasReceive
              chainId = chainId }
            |> Ok)
  

    ///
    /// Returns an UndeployedContract for use in `deployEthContract`. Emits Web3Errors in a variety of circumstances,
    /// such as malformed ABI, collisions in the function hashes, and issues with constructors and arguments.
    /// 
    /// * `env`: a `Web3Environment`
    /// * `bytecode`: a `RawContractBytecode` representing the bytecode to be deployed.
    /// * `constructorArguments`: An list option of EVMDatatypes representing inputs to the constructor of the contract.
    /// * `chainId`: A string in hex notation indicating the network the contract is deployed to, such as '0x1' (mainnet) 
    /// or '0x4' (rinkeby). See `Types.fs` for a set of pre-defined network aliases.
    /// * `abi`: An `ABI` associated with the deployed contract.
    /// 
    let public prepareUndeployedContract bytecode abi chainId (constructorArguments: EVMDatatype list)  =
        let digest = newKeccakDigest
        abi
        |> checkForBytecode bytecode
        |> canABIBeParsed
        |> checkForHashCollisions digest
        |> buildConstructorHash digest
        |> constructorRequireArgsAndNoneWereGiven constructorArguments
        |> constructorEmptyButArgsGiven constructorArguments
        |> Result.bind(fun (_, s) ->
            { abi = abi
              bytecode = bytecode
              chainId = chainId
              constructorArguments = constructorArguments
              stateMutability = s }
            |> Ok)
