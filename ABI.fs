namespace web3.fs

open web3.fs.Types

[<AutoOpen>]
module ABIFunctions =
    open System
    open System.Text
        
    open Helpers

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ABI Helpers
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Convenience function for mapping boolean values to 32 byte integer equivalents.
    let private convertBoolToInt b = 
        match b with
        | true -> "0000000000000000000000000000000000000000000000000000000000000001"
        | false -> "0000000000000000000000000000000000000000000000000000000000000000"
    
    
    ///
    /// Returns a string padded on the left to 32 hex bytes.
    let internal padTo32BytesLeft (s: string) = s.PadLeft(64, '0')

    
    ///
    /// Returns a string padded on the right to 32 hex bytes.
    let internal padTo32BytesRight (s: string) = s.PadRight(64, '0')

    
    ///
    /// Returns a string padded on the left to 32 hex bytes with 'f'
    /// characters, as a special case for negative integers.
    /// 
    let internal padTo32BytesLeftF (s: string) = s.PadLeft(64, 'f')

    
    ///
    /// Returns a formatted and padded string representing a EVM 'word' of 32 bytes.
    /// Intended to be used with `padTo32BytesRight` or `padTo32BytesLeft` on 
    /// `bytes` types, numeric types and `string` types. 
    /// 
    let private formatTypes (f: string -> string) s = bigint.Parse(s).ToString("X").ToLowerInvariant() |> f


    ///
    /// Returns the properly padded hexadecimal representation of a signed value.
    let private formatTypesInt s = 
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
    let private returnCurrentOffset cursor =
        (cursor * 32).ToString()
        |> formatTypes padTo32BytesLeft


    ///
    /// Convenience function for generating the bytestring word containing the
    /// number of inputs nested into a tuple type.
    /// 
    let private returnCountOfItems (items: 'a list) =
        padTo32BytesLeft (items.Length.ToString())


    ///
    /// Convenience function to compensate for two string characters being one byte
    /// of representation in the `bytes` type.
    /// 
    let private byteDivide2 i = (i / 2).ToString()


    ///
    /// Returns a 'wrapped' formatted bytestring, given a `bytes` EVM datatype. These
    /// `bytes` types are dynamically sized, and cross over every 32 bytes to a new 
    /// 'word', with right padding for bytes less than one word (32 bytes long).
    /// 
    let rec private wrapBytesAcrossWords (s: string) (acc: string list) =
        match s with
        | x when x.Length <= 64 -> acc @ [x |> padTo32BytesRight]
        | x when x.Length > 64 -> 
            let _x = x.[..63]
            let x = x.[64..]
            wrapBytesAcrossWords x [_x]
        | _ -> [""]

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ABI Handling
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Returns the proper count of items in a tuple, taking into account the contents of sized arrays, which consume
    /// several 'slots' directly without offset or counter. Strings and Bytes need special compensation in the
    /// calculation, as their space requirements are variable.
    /// 
    let private countOfArguments (evmDatatypeList: EVMDatatype list) =
        let rec countLoop (evmDatatypeList: EVMDatatype list) acc =
            match evmDatatypeList with
            | head :: tail ->
                match head with
                | BytesSzArraySz bArr -> countLoop tail (acc + bArr.Length)
                | AddressArraySz arr -> countLoop tail (acc + arr.Length)
                | Uint256ArraySz uArr -> countLoop tail (acc + uArr.Length)
                | Int256ArraySz iArr -> countLoop tail (acc + iArr.Length)
                | BoolArraySz bArr -> countLoop tail (acc + bArr.Length)
                | String s -> countLoop tail (acc + (wrapBytesAcrossWords s []).Length)
                | Bytes b -> countLoop tail (acc + (wrapBytesAcrossWords b []).Length)
                | _ -> countLoop tail (acc + 1)
            | [] -> acc
        countLoop evmDatatypeList 0
    
    
    ///
    /// Returns a formatted bytecode string for the 'data' argument in a EthParam1559Call, or other related transaction
    /// functions. The EVMDatatype list contains typed arguments for processing recursively, formatted in a nested way
    /// similar to how it is specified in Remix or other EVM development environments. 
    /// 
    /// **Warning** Currently, there is NO checking if the type specified and its data are conforming; these types are
    /// only present in order to determine how they should be formatted and processed into the argument bytecode.
    ///
    /// **Warning** Double arrays `[][]` are not supported.
    /// 
    /// **Warning** The `function` types are correct according to the docs (treated as a `bytes24`) but I have no way
    /// of confirming their correctness in Remix, as no one will tell me how to format `function` inputs into the Remix
    /// deployed contracts functionality. As such, they should be avoided until such time as it can be checked.
    ///
    /// **Warning** Integer types are all treated as the widest type. As already noted, there is no bounds-checking
    /// occuring here.
    ///
    let internal createInputByteString (evmDatatypeList: EVMDatatype list) =
        
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
        // In the simple case, this code will format the datatype according
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
        // The cursor value is then updated to reflect the size of the blob. Only
        // types that insert content at an offset location need to manipulate the
        // cursor value, usually by an accounting of their size, plus 1 for their
        // own internal count representation.
        //
        // Blob values are finally appended to the accumulator at the end of the 
        // current tuple. This is important; the computed offsets to a given dynamic
        // type are relative to the beginning of the current tuple!
        //
        // Overall, this is your standard FSharp recursive function, bent into an 
        // awkward shape due to the requirements of the output. 
        //
        // An initial version of this code incorrectly initialized the cursor to
        // the length of the input list, which corresponded to the count of top-
        // level items. This was incorrect, as it failed to account for sized
        // array types (which consume as many words as they have members) as well
        // as `Bytes` and `String`, which are variable based on how many words
        // they consume when wrapping is factored in. The top level (implied)
        // tuple, as well as all nested `Tuple` types use this calculation.
    
        let cursor = countOfArguments evmDatatypeList
                
        let rec unpackInputAndProcess list acc cursor =
            match list with
            | head :: tail ->
                match head with
                | Address a -> unpackInputAndProcess tail (acc + $"{a |> strip0x |> padTo32BytesLeft }") cursor
                
                | AddressArraySz arr -> 
                    unpackInputAndProcess tail (acc + (arr |> List.map(strip0x >> fun p -> $"{padTo32BytesLeft p}") |> String.concat "")) cursor
                
                | AddressArray arr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ arr |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesLeft}") (returnCountOfItems arr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + arr.Length + 1)
                
                | Uint256 u -> unpackInputAndProcess tail (acc + $"{formatTypes padTo32BytesLeft u}") cursor
                
                | Uint256ArraySz uArr ->
                    unpackInputAndProcess tail (acc + (uArr |> List.map(fun p -> $"{p |> formatTypes padTo32BytesLeft }") |> String.concat "")) cursor
                
                | Uint256Array uArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ uArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypes padTo32BytesLeft }") (returnCountOfItems uArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + uArr.Length + 1)
                
                | Int256 i -> unpackInputAndProcess tail (acc + $"{formatTypesInt i}") cursor
                
                | Int256ArraySz iArr ->
                    unpackInputAndProcess tail (acc + (iArr |> List.map(fun p -> $"{p |> formatTypesInt }") |> String.concat "")) cursor 
                
                | Int256Array iArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ iArr |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt }") (returnCountOfItems iArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + iArr.Length + 1)
                
                | Bool b -> unpackInputAndProcess tail (acc + convertBoolToInt b) cursor
                
                | BoolArraySz bArr -> unpackInputAndProcess tail (acc + (bArr |> List.map convertBoolToInt |> String.concat "")) cursor
                
                | BoolArray bArr -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ bArr |> List.fold (fun acc s -> $"{acc}{convertBoolToInt s}") (returnCountOfItems bArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + bArr.Length + 1)
                
                | BytesSz b -> 
                    unpackInputAndProcess tail (acc + $"{b |> strip0x |> padTo32BytesRight}") cursor
                
                | BytesSzArraySz bs -> 
                    unpackInputAndProcess tail (acc + (bs |> List.map(strip0x >> fun p -> $"{padTo32BytesRight p}") |> String.concat "")) cursor
                
                | BytesSzArray bs -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ bs |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesRight}") (returnCountOfItems bs) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + bs.Length + 1 )
                
                | Bytes bs -> 
                    let acc = acc + returnCurrentOffset cursor
                    let bs = bs |> strip0x
                    let contents = bs.Length |> byteDivide2 |> formatTypes padTo32BytesLeft |> fun s -> s + (wrapBytesAcrossWords bs [] |> String.concat "")
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64))
                
                | BytesArraySz bsArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents = (unpackInputAndProcess bsArr "" (countOfArguments bsArr))
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64 ))
                
                | BytesArray bsArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents = returnCountOfItems bsArr |> fun s -> s + (unpackInputAndProcess bsArr "" (countOfArguments bsArr))
                    let tail = tail @ [ contents |> Blob ] 
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64))
                
                | String st -> 
                    let acc = acc + returnCurrentOffset cursor
                    let bs = st |> formatToBytes
                    let contents =
                        bs.Length
                        |> byteDivide2
                        |> formatTypes padTo32BytesLeft
                        |> fun s -> s + (wrapBytesAcrossWords bs [] |> String.concat "")
                    
                    let tail = tail @ [contents |> Blob ] 
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64))
                
                | StringArraySz sArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents = (unpackInputAndProcess sArr "" (countOfArguments sArr))
                    let tail = tail @ [contents |> Blob]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64))
                
                | StringArray sArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents = returnCountOfItems sArr |> fun s -> s + (unpackInputAndProcess sArr "" (countOfArguments sArr))
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64) )
                
                | Tuple t ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents = unpackInputAndProcess t "" (countOfArguments t) 
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64)) 
                
                | TupleArraySz tArr ->
                    let acc = acc + returnCurrentOffset cursor 
                    let contents = unpackInputAndProcess tArr "" (countOfArguments tArr) 
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64)) 
                
                | TupleArray tArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents = returnCountOfItems tArr |> fun s -> s + (unpackInputAndProcess tArr "" (countOfArguments tArr))
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64)) 
                
                | Function f -> unpackInputAndProcess tail (acc + $"{f |> strip0x |> padTo32BytesRight }") cursor
                
                | FunctionArraySz fArr ->
                    unpackInputAndProcess tail (acc + (fArr |> List.map(strip0x >> fun p -> $"{padTo32BytesRight p}") |> String.concat "")) cursor
                
                | FunctionArray fArr -> 
                    let acc = acc + returnCurrentOffset cursor
                    let tail = tail @ [ fArr |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesRight}") (returnCountOfItems fArr) |> Blob ]
                    unpackInputAndProcess tail acc (cursor + fArr.Length + 1)
                
                | Blob blob -> unpackInputAndProcess tail (acc + blob) cursor
            | [] -> acc

        unpackInputAndProcess evmDatatypeList "" cursor


    ///
    /// Returns a substring for a given beginning offset, returning the entire word.
    let private emitSubstring start (blob: string) =
        blob.Substring(start, 64)
        
        
    ///
    /// Returns the substring with a hex specifier prepended. 
    let private emitSubstringPrepend0x start blob =
        emitSubstring start blob |> fun s -> s.TrimStart('0') |> prepend0x


    ///
    /// Returns the substring specifically for sized bytes, with a hex specifier prepended. 
    let private emitSubstringPrepend0xBytes start blob =
        emitSubstring start blob |> fun s -> s.TrimEnd('0') |> prepend0x
        
        
    ///
    /// Returns an integer value contained in a given substring. This assumes that the count/offset value being
    /// interpreted is not larger than a signed int. `Int32` is used because `List.init` (where this value is typically used)
    /// for whatever reason only accepts this type.
    /// 
    let private emitSubstringAsInt start blob =
        emitSubstring start blob |> fun i -> Convert.ToInt32(i, 16)


    ///
    /// Returns an integer value contained in a given substring, with a byte-length-to-char-length compensator applied.
    /// `Int32` is used because functions consuming this value assume `int`.
    /// 
    let private emitSubstringAsOffset blob start =
        emitSubstring start blob |> fun i -> Convert.ToInt32(i, 16) |> fun i -> i * 2


    ///
    /// Returns a boolean value after interpreting a substring in a boolean context. Non-0 values are taken as `true`.
    let private emitSubstringAsBool start blob =
        emitSubstring start blob
        |> function
            | "0000000000000000000000000000000000000000000000000000000000000000" -> false
            | _ -> true


    ///
    /// Returns a bigint that has been converted to a string, based upon the value contained in a substring.
    let private emitSubstringAsConvertedString start blob =
        emitSubstring start blob |> hexToBigInt |> fun s -> s.ToString()


    ///
    /// Returns the internal typed representation of an EVM function return value. This is typically the result of an
    /// `EthParam1559Call` or other RPC response. When calling a contract's function from inside web3.fs or elsewhere,
    /// the RPC response contains a value that is formatted according to the signature of the return type(s). This
    /// function consumes the return types (captured during the import of a contract using `loadDeployedContract` as
    /// well as the EVM's formatted return string.
    ///
    /// **Warning** The numeric types are represented in web3.fs as the widest version of their type, and the returned
    /// types will thus all be `__256`. However, the values contained therein will retain their original size.
    ///
    /// **Warning** Multidimensional arrays and the `Function` types aren't supported.
    ///  
    let internal createOutputEvmTypes (evmList: EVMDatatype list) (evmOutput: string) =
            
        // The general form of these cases is to take in the current position of the work in the `evmOutput` string
        // derive a floating 'offset' value from it, and then to unpack/manipulate the contents of the string as
        // needed for each data type's context.
        //
        // The sized array types have additional math to account for their contents, because unlike the dynamic
        // array types, they don't leave an offset at which their contents are stored, but instead the contents are
        // placed directly at the beginning cursor position. Thus they must advance the cursor more than one 'word'.
        //
        // Bytes (and by extension String) types are even more involved, since they can wrap 'words' and thus
        // extracting their contents is more complex. The Bytes array types also function with a faked offset value,
        // so placed so that the Bytes handler works properly.
        
        let rec unpackOutputAndProcess evmList evmOutput acc cursor = 
            match evmList with
            | head :: tail ->
                match head with
                | Tuple t ->
                    let offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let tupleContents = (0, offset) |> evmOutput.Remove
                    let acc = acc @ [EVMDatatype.Tuple (unpackOutputAndProcess t tupleContents [] 0)]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | TupleArraySz tArr ->
                    let offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let tupleContents = (0, offset) |> evmOutput.Remove
                    let acc = acc @ [TupleArraySz (unpackOutputAndProcess tArr tupleContents [] 0)]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | TupleArray tArr ->
                    let offset = ((cursor * 64) |> emitSubstringAsOffset evmOutput) + 64
                    let tupleContents = (0, offset) |> evmOutput.Remove
                    let acc = acc @ [TupleArray (unpackOutputAndProcess tArr tupleContents [] 0)]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Address _ ->
                    let acc = acc @ [(Address $"{emitSubstringPrepend0x (cursor * 64) evmOutput}")]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | AddressArraySz uArr ->
                    let mutable offset = (cursor * 64)
                    let arrayContents =
                        List.init uArr.Length (fun count ->
                            offset <- offset + (count * 64)
                            $"{emitSubstringPrepend0x offset evmOutput}")
                    let acc = acc @ [AddressArraySz arrayContents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + uArr.Length)
                
                | AddressArray _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput 
                    let count = emitSubstringAsInt offset evmOutput 
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringPrepend0x offset evmOutput}")
                    let acc = acc @ [AddressArray contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Int256 _ ->
                    let pointer = cursor * 64
                    let acc = acc @ [(Int256 $"{emitSubstringAsConvertedString pointer evmOutput}")]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Int256ArraySz iArr ->
                    let mutable offset = (cursor * 64)
                    let contents = List.init iArr.Length (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedString offset evmOutput}")
                    let acc = acc @ [Int256ArraySz contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + iArr.Length)
                
                | Int256Array _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = emitSubstringAsInt offset evmOutput
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedString offset evmOutput}")
                    let acc = acc @ [Int256Array contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Uint256 _ ->
                    let pointer = cursor * 64
                    let acc = acc @ [(Uint256 $"{emitSubstringAsConvertedString pointer evmOutput}")]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Uint256ArraySz uArr ->
                    let mutable offset = (cursor * 64)
                    let contents = List.init uArr.Length (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedString offset evmOutput}")
                    let acc = acc @ [Uint256ArraySz contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Uint256Array _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = emitSubstringAsInt offset evmOutput
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedString offset evmOutput}")
                    let acc = acc @ [Uint256Array contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Bool _ ->
                    let pointer = cursor * 64
                    let acc = acc @ [Bool (emitSubstringAsBool pointer evmOutput)]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | BoolArraySz bArr ->
                    let mutable offset = (cursor * 64)              
                    let contents = List.init bArr.Length (fun count ->
                        offset <- offset + (count * 64)
                        emitSubstringAsBool offset evmOutput)
                    let acc = acc @ [BoolArraySz contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + bArr.Length)  
                
                | BoolArray _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = emitSubstringAsInt offset evmOutput
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        emitSubstringAsBool offset evmOutput)
                    let acc = acc @ [BoolArray contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | BytesSz _ ->
                    let pointer = cursor * 64
                    let acc = acc @ [BytesSz $"{emitSubstringPrepend0xBytes pointer evmOutput}"]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | BytesSzArraySz bArr -> 
                    let mutable offset = (cursor * 64)
                    let contents = List.init bArr.Length (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringPrepend0xBytes offset evmOutput}")
                    let acc = acc @ [BytesSzArraySz contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + bArr.Length)
                
                | BytesSzArray _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = (emitSubstringAsInt (cursor * 64) evmOutput)
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringPrepend0xBytes offset evmOutput}")
                    let acc = acc @ [BytesSzArray contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Bytes _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = 2 * emitSubstringAsInt offset evmOutput
                    offset <- offset + 64
                    let contents = $"{(offset, count) |> evmOutput.Substring |> prepend0x}"
                    let acc = acc @ [Bytes contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | BytesArraySz bArr ->
                    let pointer = (cursor * 64) |> emitSubstringAsOffset evmOutput 
                    let inner =
                        List.init bArr.Length (fun count ->
                            let offset = (emitSubstringAsInt (pointer + (count * 64)) evmOutput) * 2
                            let byteLength =  emitSubstringAsInt (pointer + offset) evmOutput * 2
                            let arrayContents = 
                                fakedOffset + 
                                emitSubstring (pointer + offset) evmOutput + 
                                evmOutput.Substring((pointer + offset + 64), byteLength) 
                            unpackOutputAndProcess [Bytes ""] arrayContents [] 0)
                        |> List.concat
                    let acc = acc @ [BytesArraySz inner]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | BytesArray _ ->
                    let mutable pointer = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = emitSubstringAsInt pointer evmOutput
                    pointer <- pointer + 64
                    let inner =
                        List.init count (fun count ->
                            let offset = emitSubstringAsInt (pointer + (count * 64)) evmOutput * 2
                            let byteLength =  emitSubstringAsInt (pointer + offset) evmOutput * 2
                            let arrayContents =
                                fakedOffset +
                                emitSubstring (pointer + offset) evmOutput +
                                evmOutput.Substring((pointer + offset + 64), byteLength)
                            unpackOutputAndProcess [Bytes ""] arrayContents [] 0)
                        |> List.concat
                    let acc = acc @ [BytesArray inner]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | String _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = 2 * emitSubstringAsInt offset evmOutput
                    offset <- offset + 64
                    let contents =
                        $"{evmOutput.Substring(offset, count)
                           |> Convert.FromHexString
                           |> Encoding.UTF8.GetString}"
                    let acc = acc @ [EVMDatatype.String contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | StringArraySz sArr ->
                    let pointer = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let inner =
                        List.init sArr.Length (fun count ->
                            let offset = emitSubstringAsInt (pointer + (count * 64)) evmOutput * 2
                            let byteLength =  emitSubstringAsInt (pointer + offset) evmOutput * 2
                            let arrayContents = 
                                fakedOffset +
                                emitSubstring (pointer + offset) evmOutput + 
                                evmOutput.Substring((pointer + offset + 64), byteLength) 
                            unpackOutputAndProcess [EVMDatatype.String ""] arrayContents [] 0)
                        |> List.concat
                    let acc = acc @ [StringArraySz inner]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | StringArray _ ->
                    let mutable pointer = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = emitSubstringAsInt pointer evmOutput
                    pointer <- pointer + 64
                    let contents =
                        List.init count (fun count ->
                            let offset = emitSubstringAsInt (pointer + (count * 64)) evmOutput * 2
                            let byteLength =  emitSubstringAsInt (pointer + offset) evmOutput * 2
                            let arrayContents =
                                fakedOffset +
                                emitSubstring (pointer + offset) evmOutput +
                                evmOutput.Substring((pointer + offset + 64), byteLength)
                            unpackOutputAndProcess [Bytes ""] arrayContents [] 0)
                        |> List.concat
                    let acc = acc @ [StringArray contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | _ -> unpackOutputAndProcess tail evmOutput acc (cursor + 1)
            | [] -> acc
        
        unpackOutputAndProcess evmList (evmOutput |> strip0x ) [] 0

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ABI Type Checking
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    ///
    /// Returns a boolean indicating whether the hexadecimal string representation of numeric EVM output is able to be
    /// represented by the indicated type. Hexadecimal strings should begin with '0x'. Use `prepend0x` if unsure.
    /// 
    let public boundsCheck (sem: EVMTypeSignaling) s =
        match sem with
        | EVMUint8 -> s |> hexToBigInt |> fun i -> not(i > 255I) && not(i < 0I) 
        | EVMUint16 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 16)) && not(i < 0I) 
        | EVMUint32 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 32)) && not(i < 0I) 
        | EVMUint64 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 64)) && not(i < 0I) 
        | EVMUint128 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 128)) && not(i < 0I) 
        | EVMInt8 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 7) - 1I) && not(i < - (bigint.Pow(2, 7)))
        | EVMInt16 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 15) - 1I) && not(i < - (bigint.Pow(2, 15))) 
        | EVMInt32 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 15) - 1I) && not(i < - (bigint.Pow(2, 15))) 
        | EVMInt64 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 31) - 1I) && not(i < - (bigint.Pow(2, 31))) 
        | EVMInt128 -> s |> hexToBigInt |> fun i -> not(i > bigint.Pow(2, 63) - 1I) && not(i < - (bigint.Pow(2, 63))) 
         
        
    ///
    /// Returns the input EVMDatatype list if the inputs conform to some basic checks, otherwise an error string that
    /// will bubble up later in the pipeline.
    ///
    let internal checkEVMData evmDataList =
        let rec checkEVMDataConforming evmDataList =
            match evmDataList with
            | head :: tail ->
                match head with 
                | Uint256 u ->
                    if (bigint.Parse(u) |> fun i -> not(i > bigint.Pow(2, 256)) && not(i < 0I)) then checkEVMDataConforming tail
                    else "Specified unsigned integer exceeded the maximum value or was less than 0"
                        |> DataValidatorError
                        |> Error
                | Int256 i ->
                    if (bigint.Parse(i) |> fun i -> not(i > bigint.Pow(2, 127) - 1I) && not(i < - (bigint.Pow(2, 127)))) then checkEVMDataConforming tail
                    else "Specified signed integer exceeded the maximum values"
                         |> DataValidatorError
                         |> Error
                | Address a ->
                    a
                    |> strip0x
                    |> fun s ->
                        if s.Length % 40 = 0 then checkEVMDataConforming tail
                        else "Specified Address length incorrect"
                             |> DataValidatorError
                             |> Error
                | Bytes b ->
                    b
                    |> strip0x
                    |> fun s ->
                        if s.Length % 2 = 0 then checkEVMDataConforming tail
                        else "Bytes length must be even, i.e. the specified bytes must be in pairs."
                             |> DataValidatorError
                             |> Error
                | BytesSz b ->
                    b
                    |> strip0x
                    |> fun s ->
                        if s.Length <= 64 && s.Length % 2 = 0 then checkEVMDataConforming tail
                        else "Sized bytes must be less than or equal to 32 bytes (64 characters) and in pairs."
                             |> DataValidatorError
                             |> Error
                | _ -> checkEVMDataConforming tail
            | [] -> CheckedSuccess |> Ok
        
        checkEVMDataConforming evmDataList
        
                    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ABI Binds
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Returns the string representation of the wrapped EVMData value, except for bool types, which are handled in
    /// `unwrapEVMBool`. EVMDatatypes that are lists return a comma-separated concatenated string.
    let rec public unwrapEVMValue (evmDataType: EVMDatatype) =
        match evmDataType with
        | Address a -> a
        | AddressArraySz a -> a |> String.concat(",")
        | AddressArray a -> a |> String.concat(",")
        | Uint256 u -> u
        | Uint256ArraySz a -> a |> String.concat(",")
        | Uint256Array a -> a |> String.concat(",")
        | Int256 u -> u
        | Int256ArraySz a -> a |> String.concat(",")
        | Int256Array a -> a |> String.concat(",")
        | BytesSz b -> b
        | BytesSzArraySz a -> a |> String.concat(",")
        | BytesSzArray a -> a |> String.concat(",")
        | Bytes b -> b
        | BytesArraySz b ->  b |> List.map unwrapEVMValue |> String.concat(",")
        | BytesArray b -> b |> List.map unwrapEVMValue |> String.concat(",")
        | Function f -> f
        | FunctionArraySz l -> l |> String.concat(",")
        | FunctionArray l -> l |> String.concat(",")
        | String s -> s
        | StringArraySz b ->  b |> List.map unwrapEVMValue |> String.concat(",")
        | StringArray b -> b |> List.map unwrapEVMValue |> String.concat(",")
        | _ -> ""
    
    
    ///
    /// Returns a function's outputs from the EVM as an EVMDatatype list
    let internal returnOutputAsEVMDatatypes contract evmFunction output =
        bindFunctionIndicator contract evmFunction
        |> fun s -> createOutputEvmTypes s.internalOutputs output
     
     
    ///
    /// Returns the underlying boolean contained in a wrapped Bool EVMDatatype. For obvious reasons, bools from `Bool`
    /// will return a singleton List.
    let internal unwrapEVMBool (evmBools: EVMDatatype) =
        match evmBools with
        | Bool b -> [b]
        | BoolArraySz b -> b
        | BoolArray b -> b
        | _ -> []
        
    ///
    /// Returns a wrapped EthAddress that has been checked for validity 
    let internal wrapEthAddress (address: string) =
        match [address |> Address] |> checkEVMData with
        | Ok _ -> address |> EthAddress |> Ok
        | Error _ -> EthAddressError |> Error
    
    
    ///
    /// Convenience function that returns a ContractConstants that contains the address used for the session, along
    /// with other values ready for manipulation via the `with` statement for modifying records. If the RPC is a wallet,
    /// these defaults should work perfectly well. If the RPC is an actual Ethereum node, the gas values and transaction
    /// type should be changed as required.
    /// 
    let public createDefaultConstants (address: string) =
        {
        walletAddress = address |> EthAddress
        transactionType = None
        maxFeePerGas = None
        maxPriorityFeePerGas = None
        data = None
        blockHeight = Some LATEST
        defaultValue = Some "0"
        }
