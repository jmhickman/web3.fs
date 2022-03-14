namespace web3.fs

module ContractFunctions =
    open System
    open FSharp.Data
    open SHA3Core.Keccak

    open Helpers
    open Types

    ///
    /// Returns a function selector given a configured Keccak instance and the
    /// canonical representation of a function.
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
    
    let bindCanonicalRepresentation a =
        match a with
        | CanonicalFunctionRepresentation s -> s
        | CanonicalEventRepresentation s -> s
        | CanonicalErrorRepresentation s -> s
        
        
    ///
    /// Returns upwrapped EVMFunctionInput string
    let bindEVMFunctionInputs a = function EVMFunctionInputs s -> s
    
    
    ///
    /// Returns upwrapped EVMFunctionOutput string
    let bindEVMFunctionOutputs a = function EVMFunctionOutputs s -> s
    
    
    ///
    /// Returns the text of the "type" property.
    let getInnerTypeText (jVal: JsonValue) = jVal.GetProperty("type").InnerText()

    ///
    /// Predicate for filters
    let testPropertyInnerText (s: string) (jVal: JsonValue) =
        jVal.GetProperty("type").InnerText() = s

    ///
    /// Checks if the JsonValue 'type' value is tuple. Tupled values are treated differently in the logic.
    let checkForTuple (jVal: JsonValue) =
        if (getInnerTypeText jVal).StartsWith("tuple") then
            true
        else
            false

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


    ///
    /// Returns the anonymous boolean of an EVM event, given a JsonValue option. Defaults to false.
    let returnAnonymous (b: JsonValue option) =
        match b with
        | Some jVal ->
            if jVal.AsBoolean() = true then
                true
            else
                false
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
          outputs = returnInputs _outputs |> EVMFunctionOutputs
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
              deployedConstructorArguments = ""
              fallback = fallback
              receive = receive
              chainId = chainId }
            |> Ok
        | None ->
            ContractParseFailure "Json was incorrectly formatted or otherwise failed to parse"
            |> Error
