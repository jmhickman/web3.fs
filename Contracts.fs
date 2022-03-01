namespace web3.fs

module ContractFunctions =
    open System
    open FSharp.Data
    open SHA3Core.Keccak

    open Helpers
    open Types


    ///
    /// Performs ABI parsing and emits the AsArray() for of the output
    let parseABIAndEmitArray = fun abi -> JsonValue.Parse(abi) |> fun a -> a.AsArray()


    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for functions and returns
    /// an Option for the name, inputs, outputs and the state mutability parameter.
    ///
    let tryGetFunctionProperties (jvals: JsonValue array) =
        jvals
        |> Array.filter (fun i -> i.GetProperty("type").InnerText() = "function") // Just functions now
        |> Array.map (fun i ->
            (i.TryGetProperty("name"),
             i.TryGetProperty("inputs"),
             i.TryGetProperty("outputs"),
             i.TryGetProperty("statemutability")))

    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for events and returns
    /// an option for the name, inputs, and the anonymous parameter.
    ///
    let tryGetEventProperties (jvals: JsonValue array) =
        jvals
        |> Array.filter (fun i -> i.GetProperty("type").InnerText() = "event")
        |> Array.map (fun i -> (i.TryGetProperty("name"), i.TryGetProperty("inputs"), i.TryGetProperty("anonymous")))

    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for the constructor and
    /// returns its inputs. Constructors never have a name or outputs. Only useful for deploying a contract
    /// programmatically.
    ///
    let tryGetConstructorProperties (jvals: JsonValue array) =
        jvals
        |> Array.filter (fun p -> p.GetProperty("type").InnerText() = "constructor")
        |> Array.map (fun i -> (i.TryGetProperty("inputs")))

    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'receive' function. Receive functions have no inputs, must be 'payable', and may not have outputs.
    /// Thus, this function only returns the presence of a receive function only.
    ///
    let tryGetReceive (jvals: JsonValue array) =
        jvals
        |> Array.filter (fun p -> p.GetProperty("type").InnerText() = "receive")
        |> Array.tryHead


    ///
    /// Given a JsonValue parsed output of an exported ABI, this discovers the presence of an explicit
    /// 'fallback' function. The fallback may only have an empty argument tuple, or a bytes argument
    /// that the EVM will fill with the calldata of the txn that hit the fallback. It may only return a
    /// bytes as output. It may be payable.
    let tryGetFallback (jvals: JsonValue array) =
        jvals
        |> Array.filter (fun p -> p.GetProperty("type").InnerText() = "fallback")
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
    /// Defaults to 'nonpayble' as is the spec.
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
    ///
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
          inputs = inputs
          outputs = returnInputs _outputs
          config = returnStateMutability _stateMut }


    ///
    /// When supplied with an IntermediateEventRepresentation, returns the corresponding EVMEvent record.
    let returnEVMEvent (digest: Keccak) (interEvent: IntermediateEventRepresentation) =

        let _evntName, _inputs, _anon = interEvent

        let name = returnFunctionName _evntName
        let anon = returnAnonymous _anon
        let inputs = returnInputs _inputs

        let hash =
            ($"{name}{inputs}" |> CanonicalEventRepresentation)
            |> returnFunctionSelector digest

        { name = name
          anonymous = anon
          inputs = inputs
          hash = hash }

    ///
    /// When supplied with a Contracts ABI in Json format, returns a list of all functions as
    /// EVMFunctions. A Keccak hash digest is required to generate function selectors. It is
    /// recommended to create a digest and then partially apply this function for convenience
    ///
    let parseABIForFunctions (digest: Keccak) (json: string) =
        json
        |> parseABIAndEmitArray
        |> tryGetFunctionProperties
        |> Array.map (returnEVMFunction digest)
        |> Array.toList


    ///
    /// When supplied with a Contracts ABI in Json format, returns a list of all events as
    /// EVMEvents. A Keccak hash digest is required to generate event selectors. It is
    /// recommended to create a digest and then partially apply this function for convenience
    ///
    let parseABIForEvents (digest: Keccak) (json: string) =
        json
        |> parseABIAndEmitArray
        |> tryGetEventProperties
        |> Array.map (returnEVMEvent digest)
