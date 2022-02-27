namespace web3.fs

module ContractFunctions =
    open System
    open FSharp.Data

    open Helpers
    open Types

    ///
    /// Performs ABI parsing and emits the AsArray() for of the output
    ///
    let parseABIAndEmitArray = fun abi -> JsonValue.Parse(abi) |> fun a -> a.AsArray()

    ///
    /// Given the JsonValue parsed output of an exported ABI, this filters the ABI for functions and returns
    /// and Option for the name and any Inputs.
    ///
    // This will likely be expanded to include outputs later.
    let tryGetFunctionNamesAndInputs (jvals: JsonValue array) =
        jvals
        |> Array.filter (fun i -> i.GetProperty("type").ToString() = "\"function\"") // Just functions now
        |> Array.map (fun i -> (i.TryGetProperty("name"), i.TryGetProperty("inputs")))


    ///
    /// When supplied with a tuple of a JsonValue containing the function name and a JsonValue containing the inputs,
    /// flattens the inputs into the canonical representation of the function, in order to be hashed into the function
    /// selector.
    ///
    let collapseTuples (interFunc: IntermediateFunction) =

        let funcName, _inputs = interFunc

        /// Recursively send tupled components through the function in order to extract and format nested values properly.
        /// Otherwise simply concatenate the strings together. ()'s inserted as needed.
        let rec buildFunctionString (_input: JsonValue) =
            match _input with
            | JsonValue.Array elements ->
                elements
                |> Array.map (fun e ->
                    if checkForTuple e then
                        $"""{buildFunctionString (e.GetProperty("components"))}{extractEnder e}"""
                    else
                        $"""{getInnerTypeText e}""")
                |> fun s -> String.Join(',', s)
                |> fun s -> $"({s})"
            | _ -> ""

        ///
        /// Names are an Option for now, since I've received ABI files with functions lacking names somehow.
        /// Unsure if empty quotes are the best way to handle this or not...
        ///

        let _funcName =
            match funcName with
            | Some f -> f.InnerText()
            | None -> ""

        ///
        /// Some functions have no Inputs, and will simply move through here with an empty string
        ///
        let inputBind (funcName: string) (_val: JsonValue option) =
            match _val with
            | Some jVal -> $"""{funcName}{buildFunctionString jVal}"""
            | None -> $"""{funcName}()"""

        inputBind _funcName _inputs
