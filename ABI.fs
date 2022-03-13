namespace web3.fs

module ABIFunctions =
    
    open Types
    open Helpers

    //
    // ABI Helpers
    //

    ///
    /// Convenience function for mapping boolean values to 32 byte integer equivalents.
    let convertBoolToInt b = 
        match b with
        | true -> "0000000000000000000000000000000000000000000000000000000000000001"
        | false -> "0000000000000000000000000000000000000000000000000000000000000000"
    
    ///
    /// Returns a string padded on the left to 32 hex bytes.
    let padTo32BytesLeft (s: string) = s.PadLeft(64, '0')

    ///
    /// Returns a string padded on the right to 32 hex bytes.
    let padTo32BytesRight (s: string) = s.PadRight(64, '0')

    ///
    /// Returns a string padded on the left to 32 hex bytes with 'f'
    /// characters, as a special case for negative integers.
    /// 
    let padTo32BytesLeftF (s: string) = s.PadLeft(64, 'f')

    ///
    /// Returns a formatted and padded string representing a EVM 'word' of 32 bytes.
    /// Intended to be used with `padTo32BytesRight` or `padTo32BytesLeft` on 
    /// `bytes` types, numeric types and `string` types. 
    /// 
    let formatTypes (f: string -> string) s = bigint.Parse(s).ToString("X").ToLowerInvariant() |> f

    ///
    /// Returns the properly padded hexadecimal representation of a signed value.
    let formatTypesInt s = 
        let int' = bigint.Parse(s)
        match int' with
        | x when x.Sign = -1 -> 
            x.ToString("X").ToLowerInvariant() |> padTo32BytesLeftF
        | x when x.Sign >= 0 ->
            x.ToString("X").ToLowerInvariant() |> padTo32BytesLeft
        | x -> x.ToString("X").ToLowerInvariant() |> padTo32BytesLeft
        

    ///
    /// Find the hexadecimal representation of the current 'offset'/position
    /// of the 'cursor,' the abstraction used to track the next available slot
    /// to insert dynamic type arguments.
    /// 
    let returnCurrentOffset cursor =
        (cursor * 32).ToString()
        |> formatTypes padTo32BytesLeft


    ///
    /// Convenience function for generating the bytestring word containing the
    /// number of inputs nested into a tuple type.
    /// 
    let returnCountOfItems (items: 'a list) =
        padTo32BytesLeft (items.Length.ToString())

    ///
    /// Convenience function to compensate for two string characters being one byte
    /// of representation in the `bytes` type.
    /// 
    let byteDivide2 i = (i / 2).ToString()

    ///
    /// Returns a 'wrapped' formatted bytestring, given a `bytes` EVM datatype. These
    /// `bytes` types are dynamically sized, and cross over every 32 bytes to a new 
    /// 'word', with right padding for bytes less than one word (32 bytes long).
    /// 
    let rec wrapBytesAcrossWords (s: string) (acc: string list) =
        match s with
        | x when x.Length < 64 -> acc @ [x |> padTo32BytesRight]
        | x when x.Length > 64 -> 
            let _x = x.[..63]
            let x = x.[64..]
            wrapBytesAcrossWords x [_x]
        | _ -> [""]

    
    ///
    /// Returns a formatted bytecode string for the 'data' argument in a EthParam1559Call, 
    /// or other related transaction functions. The EVMDatatype list contains typed 
    /// arguments for processing recursively, formatted in a nested way similar to how 
    /// it is specified in Remix or other EVM development environments. 
    /// 
    /// **Warning** Currently, there is NO checking if the type specified and its data are 
    /// conforming; these types are only present in order to determine how they should be 
    /// formatted and processed into the argument bytecode.
    ///
    /// **Warning** Double arrays `[][]` of the same type are not supported, unless they are
    /// tuple arrays. 
    /// 
    /// **Warning** The `function` types are correct according to the docs (treated as a 
    /// `bytes24`) but I have no way of confirming their correctness in Remix, as no one
    /// will tell me how to format `function` inputs into the Remix deployed contracts 
    /// interaction functionality.
    ///
    let createInputByteString (evmDatatypeList: EVMDatatype list) =
        
        // A description of the following code is in order, I think. 
        //
        // A typical 'canonical' function signature might look like this:
        // `someFunction(uint256,address,bytes24,int64)`
        //
        // A simple type is like `address`, `uint256`, and sized byte sequences
        // like `bytes24`. 
        //
        // Dynamic types are arrays of simple types or tuples, and `bytes` 
        // sequences, like `address[]`, `uint256[]` and `(address,uint256)[]`
        //
        // The EVM expects function call arguments to be placed in a
        // certain order. Most simple types are placed directly in sequence
        // based on their order in the function signature. However, dynamic 
        // types require extra work. They leave an offset to their location 
        // in the argument string, and the contents are only appended after all other
        // simple values in the current tuple are exhausted. The EVM will look for
        // the dynamic contents at the offset value, where it expects to 
        // find a count of the members of the dynamic type (a true count in most 
        // cases except `bytes`, which are a count of the hex bytes instead).
        //
        // In the simple case, this code will simply format the datatype according
        // to the rules, and place it in order as it was encountered. In sized 
        // array cases, the logic is much the same, repeated for all members of the
        // array. 
        //
        // Due to their extra complication, dynamic types carry the notion of the 
        // 'cursor.' This is a simple abstraction that tracks the next available 
        // 'slot' that a blob of data may be placed. The cursor is initialized
        // beyond the end of the array, which looks strange but is the desired 
        // behavior, as this is the first 'slot' to place a dynamic type's contents.
        //
        // The code then proceeds to grab the current cursor's value and place it
        // into a EVM word, and then it goes about building the 'blob' that contains
        // the real values. This is appended as more work on the current `tail`. 
        // The cursor value is then updated to reflect the size of the blob.
        // Blob values are finally appended to the accumulator at the end of the 
        // current tuple. This is important; the computed offsets to a given dynamic
        // type are relative to the beginning of the current tuple!
        //
        // Overall, this is your standard FSharp recursive function, bent into an 
        // awkward shape due to the requirements of the output. 
        //
        // If this description was too opaque, that's okay. There is a collection of
        // inputs and corresponding EVM bytecode outputs at the end of this source
        // file to help those more visually inclined to understand.
    
        let cursor = evmDatatypeList.Length


        let rec unpackInputAndProcess list (acc: string) (cursor: int) : string =
            match list with
            | head :: tail ->
                match head with
                | Address a -> unpackInputAndProcess tail (acc + $"{a |> strip0x |> padTo32BytesLeft }") cursor
                | AddressArraySz arr -> 
                    unpackInputAndProcess tail (acc + (arr |> List.map(strip0x >> fun p -> $"{padTo32BytesLeft p}") |> String.concat "")) cursor
                | AddressArray arr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ arr |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesLeft}") (returnCountOfItems arr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + arr.Length)
                | Uint8 u -> 
                    unpackInputAndProcess tail (acc + $"{formatTypes padTo32BytesLeft u}") cursor
                | Uint32 u -> 
                    unpackInputAndProcess tail (acc + $"{formatTypes padTo32BytesLeft u}") cursor
                | Uint64 u -> 
                    unpackInputAndProcess tail (acc + $"{formatTypes padTo32BytesLeft u}") cursor
                | Uint128 u ->
                    unpackInputAndProcess tail (acc + $"{formatTypes padTo32BytesLeft u}") cursor
                | Uint256 u ->
                    unpackInputAndProcess tail (acc + $"{formatTypes padTo32BytesLeft u}") cursor
                | Uint8ArraySz uArr -> 
                    unpackInputAndProcess tail (acc + (uArr |> List.map(fun p -> $"{p |> formatTypes padTo32BytesLeft }") |> String.concat "")) cursor
                | Uint32ArraySz uArr ->
                    unpackInputAndProcess tail (acc + (uArr |> List.map(fun p -> $"{p |> formatTypes padTo32BytesLeft }") |> String.concat "")) cursor
                | Uint64ArraySz uArr ->
                    unpackInputAndProcess tail (acc + (uArr |> List.map(fun p -> $"{p |> formatTypes padTo32BytesLeft }") |> String.concat "")) cursor
                | Uint128ArraySz uArr ->
                    unpackInputAndProcess tail (acc + (uArr |> List.map(fun p -> $"{p |> formatTypes padTo32BytesLeft }") |> String.concat "")) cursor
                | Uint256ArraySz uArr ->
                    unpackInputAndProcess tail (acc + (uArr |> List.map(fun p -> $"{p |> formatTypes padTo32BytesLeft }") |> String.concat "")) cursor
                | Uint8Array uArr -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ uArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypes padTo32BytesLeft }") (returnCountOfItems uArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + uArr.Length)
                | Uint32Array uArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ uArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypes padTo32BytesLeft }") (returnCountOfItems uArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + uArr.Length)
                | Uint64Array uArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ uArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypes padTo32BytesLeft }") (returnCountOfItems uArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + uArr.Length)
                | Uint128Array uArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ uArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypes padTo32BytesLeft }") (returnCountOfItems uArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + uArr.Length)
                | Uint256Array uArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ uArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypes padTo32BytesLeft }") (returnCountOfItems uArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + uArr.Length)
                | Int8 i -> 
                    unpackInputAndProcess tail (acc + $"{formatTypesInt i}") cursor
                | Int32 i -> 
                    unpackInputAndProcess tail (acc + $"{formatTypesInt i}") cursor
                | Int64 i -> 
                    unpackInputAndProcess tail (acc + $"{formatTypesInt i}") cursor
                | Int128 i -> 
                    unpackInputAndProcess tail (acc + $"{formatTypesInt i}") cursor
                | Int256 i -> 
                    unpackInputAndProcess tail (acc + $"{formatTypesInt i}") cursor
                | Int8ArraySz iArr -> 
                    unpackInputAndProcess tail (acc + (iArr |> List.map(fun p -> $"{p |> formatTypesInt }") |> String.concat "")) cursor
                | Int32ArraySz iArr ->
                    unpackInputAndProcess tail (acc + (iArr |> List.map(fun p -> $"{p |> formatTypesInt }") |> String.concat "")) cursor
                | Int64ArraySz iArr ->
                    unpackInputAndProcess tail (acc + (iArr |> List.map(fun p -> $"{p |> formatTypesInt }") |> String.concat "")) cursor
                | Int128ArraySz iArr ->
                    unpackInputAndProcess tail (acc + (iArr |> List.map(fun p -> $"{p |> formatTypesInt }") |> String.concat "")) cursor
                | Int256ArraySz iArr ->
                    unpackInputAndProcess tail (acc + (iArr |> List.map(fun p -> $"{p |> formatTypesInt }") |> String.concat "")) cursor
                | Int8Array iArr -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ iArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt }") (returnCountOfItems iArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + iArr.Length)
                | Int32Array iArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ iArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt }") (returnCountOfItems iArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + iArr.Length)
                | Int64Array iArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ iArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt }") (returnCountOfItems iArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + iArr.Length)
                | Int128Array iArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ iArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt }") (returnCountOfItems iArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + iArr.Length)
                | Int256Array iArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ iArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt }") (returnCountOfItems iArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + iArr.Length)
                | Bool b -> unpackInputAndProcess tail (acc + convertBoolToInt b) cursor
                | BoolArraySz bArr -> unpackInputAndProcess tail (acc + (bArr |> List.map convertBoolToInt |> String.concat "")) cursor
                | BoolArray bArr -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ bArr |> List.fold (fun acc s -> $"{acc}{convertBoolToInt s}") (returnCountOfItems bArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + bArr.Length)
                | BytesSz b -> 
                    unpackInputAndProcess tail (acc + $"{b |> strip0x |> padTo32BytesRight}") cursor
                | BytesSzArraySz bs -> 
                    unpackInputAndProcess tail (acc + (bs |> List.map(strip0x >> fun p -> $"{padTo32BytesRight p}") |> String.concat "")) cursor
                | BytesSzArray bs -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ bs |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesRight}") (returnCountOfItems bs) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + bs.Length)
                | Bytes bs -> 
                    let acc = acc + returnCurrentOffset cursor
                    let bs = bs |> strip0x
                    let blob = bs.Length |> byteDivide2 |> formatTypes padTo32BytesLeft |> fun s -> s + (wrapBytesAcrossWords bs [] |> String.concat "")
                    let tail = tail @ [ blob |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (blob.Length / 64))
                | BytesArraySz bsArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ (unpackInputAndProcess bsArr "" (0 + bsArr.Length)) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + bsArr.Length)
                | BytesArray bsArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ returnCountOfItems bsArr |> fun s -> s + (unpackInputAndProcess bsArr "" (0 + bsArr.Length)) |> Blob ] 
                    unpackInputAndProcess tail acc (cursor + bsArr.Length)
                | String st -> 
                    let acc = acc + returnCurrentOffset cursor
                    let bs = st |> formatToBytes
                    let blob = bs.Length |> byteDivide2 |> formatTypes padTo32BytesLeft |> fun s -> s + (wrapBytesAcrossWords bs [] |> String.concat "")
                    let tail = tail @ [ blob |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (blob.Length / 64))
                | StringArraySz sArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ (unpackInputAndProcess sArr "" (0 + sArr.Length)) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + sArr.Length)
                | StringArray sArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ returnCountOfItems sArr |> fun s -> s + (unpackInputAndProcess sArr "" (0 + sArr.Length)) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + sArr.Length)
                | Tuple t ->
                    let acc = acc + returnCurrentOffset cursor
                    let blob = unpackInputAndProcess t "" (0 + t.Length) 
                    let tail = tail @ [ blob |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (blob.Length / 64))
                | TupleArray ta ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ returnCountOfItems ta |> fun s -> s + (unpackInputAndProcess ta "" (0 + ta.Length)) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + ta.Length)
                | Function f -> 
                    unpackInputAndProcess tail (acc + $"{f |> strip0x |> padTo32BytesRight }") cursor
                | FunctionArraySz farr ->
                    unpackInputAndProcess tail (acc + (farr |> List.map(strip0x >> fun p -> $"{padTo32BytesRight p}") |> String.concat "")) cursor
                | FunctionArray fArr -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ fArr |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesRight}") (returnCountOfItems fArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + fArr.Length)
                | Blob blob -> unpackInputAndProcess tail (acc + blob) cursor
            | [] -> acc

        unpackInputAndProcess evmDatatypeList "" cursor

(*
    let test00 =
        [Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]];Tuple[TupleArray[Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]];Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]]];Uint256 "512"]]
    
    let test000 =
        [Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]];Tuple[TupleArray[Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]];Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]]];Uint256 "512"];Tuple[Tuple[TupleArray[Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]];Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]]];Uint256 "512"];Tuple[Uint256 "0";Int8 "120";AddressArray["0x2247772b1319442401dd04812bdefd7025bffec3";"0x2247772b1319442401dd04812bdefd7025bffec3"]]]]

    let test1 = [Uint32 "69"; Bool true] 

    let test2 = [BytesSzArraySz["0x414141"; "0x424242"]] 

    let test3 = [BytesSzArray["0x414141"; "0x424242"; "0x434343"]] 

    let test4 = [Bytes "0x414141414141414141414141414141414141414141414141414141414141414141414141" ] 

    let test5 = [BytesArray[ Bytes "0x414141414141414141414141414141414141414141414141414141414141414141414141"; Bytes "0x414141414141414141414141414141414141414141414141414141414141414141414141414141414142"]] // passed

    let test6 = [BytesArraySz [Bytes "0x6162"; Bytes "0x6263"]] 

    let test7 = [StringArraySz[String "hahahaha"; String "doodyaaah"] ] 

    let test8 = [StringArray[String "hahahaha"; String "doodyaaah"] ] 

    let test9 = [BoolArraySz[true; false; false]] 

    let test10 = [BoolArray[true; true; false]] 

    let test11 = [AddressArraySz["0x2247772b1319442401dd04812bdefd7025bffec3"; "0x2247772b1319442401dd04812bdefd7025bffec3"]] 

    let test12 = [Int8Array["56";"-56"]] 

    let test13 = [Int32Array["565656"; "-565656"]] 

    let test14 = [Int64Array["5656565656565656"; "-5656565656565656"]]
    
    let test15 = [Int128Array["-565656565656565656565656565656565656"; "565656565656565656565656565656565656"]]
    
    let test16 = [Int256Array["56565656565656565656565656565656565656565656565656565656565656565656565656"; "-56565656565656565656565656565656565656565656565656565656565656565656565656"]] 

    let test17 = [Function "0x2247772b1319442401dd04812bdefd7025bffec3abcdef10"]

    test1
    0000000000000000000000000000000000000000000000000000000000000045
    0000000000000000000000000000000000000000000000000000000000000001
    test2
    4141410000000000000000000000000000000000000000000000000000000000
    4242420000000000000000000000000000000000000000000000000000000000
    test3
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000003
    4141410000000000000000000000000000000000000000000000000000000000
    4242420000000000000000000000000000000000000000000000000000000000
    4343430000000000000000000000000000000000000000000000000000000000
    test4
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000024
    4141414141414141414141414141414141414141414141414141414141414141
    4141414100000000000000000000000000000000000000000000000000000000
    test5
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000000000000000000000000000000000000000000040
    00000000000000000000000000000000000000000000000000000000000000a0
    0000000000000000000000000000000000000000000000000000000000000024
    4141414141414141414141414141414141414141414141414141414141414141
    4141414100000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000002a
    4141414141414141414141414141414141414141414141414141414141414141
    4141414141414141414200000000000000000000000000000000000000000000
    test6
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000080
    0000000000000000000000000000000000000000000000000000000000000002
    6162000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000002
    6263000000000000000000000000000000000000000000000000000000000000
    test7
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000080
    0000000000000000000000000000000000000000000000000000000000000008
    6861686168616861000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000009
    646F6F6479616161680000000000000000000000000000000000000000000000
    test8
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000080
    0000000000000000000000000000000000000000000000000000000000000008
    6861686168616861000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000009
    646F6F6479616161680000000000000000000000000000000000000000000000
    test9
    0000000000000000000000000000000000000000000000000000000000000001
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    test10
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000003
    0000000000000000000000000000000000000000000000000000000000000001
    0000000000000000000000000000000000000000000000000000000000000001
    0000000000000000000000000000000000000000000000000000000000000000
    test11
    0000000000000000000000002247772b13194424010404812bdefd7025bffec3
    0000000000000000000000002247772b13194424010404812bdefd7025bffec3
    test12
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000000000000000000000000000000000000000000038
    ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc8
    test13
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000002
    000000000000000000000000000000000000000000000000000000000008a198
    fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff75e68
    test14
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000000000000000000000000000000014189dd29bb798
    ffffffffffffffffffffffffffffffffffffffffffffffffffebe7622d644868
    test15
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000002
    ffffffffffffffffffffffffffffffffff930efa64a58f62eabdf399dbcc4868
    00000000000000000000000000000000006cf1059b5a709d15420c662433b798
    test16
    0000000000000000000000000000000000000000000000000000000000000020
    0000000000000000000000000000000000000000000000000000000000000002
    002003d8d000a8b8843b1d0d7ceb294c4c2ecc156e97db79890cede62433b798
    ffdffc272fff57477bc4e2f28314d6b3b3d133ea9168248676f31219dbcc4868

    test00
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000100
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000200
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000100
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3

    test000
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000120
    0000000000000000000000000000000000000000000000000000000000000340
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000200
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000100
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000260
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000200
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000000000000000000000000000000000000000000040
    0000000000000000000000000000000000000000000000000000000000000100
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000078
    0000000000000000000000000000000000000000000000000000000000000060
    0000000000000000000000000000000000000000000000000000000000000002
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
    0000000000000000000000002247772b1319442401dd04812bdefd7025bffec3
*)