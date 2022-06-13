namespace Web3.fs

[<AutoOpen>]
module ABIFunctions =
    open System
    open System.Text
        
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ABI Helpers
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Convenience function for mapping boolean values to 32 byte integer
    /// equivalents.
    /// 
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
    /// Returns a formatted string representing a EVM 'word' (32 bytes).
    /// Intended to be used with `padTo32BytesRight` or `padTo32BytesLeft` on 
    /// `bytes` types, numeric types and `string` types. 
    /// 
    let private formatTypes (f: string -> string) s =
        bigint.Parse(s).ToString("X").ToLowerInvariant() |> f


    ///
    /// Returns the properly padded hexadecimal representation numeric value
    /// for all integer types.
    let private formatTypesInt s = 
        let int' = bigint.Parse(s)
        match int' with
        | x when x.Sign = -1 -> 
            x.ToString("X").ToLowerInvariant() |> padTo32BytesLeftF
        | x -> x.ToString("X").ToLowerInvariant().TrimStart('0') |> padTo32BytesLeft
        

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
    /// Convenience function to compensate for two string characters being one
    /// byte of representation in the `bytes` type.
    /// 
    let private byteDivide2 i = (i / 2).ToString()


    ///
    /// Returns a 'wrapped' formatted bytestring, given a `bytes` EVM datatype.
    /// These `bytes` types are dynamically sized, and cross over every 32 bytes
    /// to a new word, with right padding for bytes less than one word
    /// (32 bytes long).
    /// 
    let rec private wrapBytesAcrossWords (s: string) (acc: string list) =
        match s with
        | x when x.Length <= 64 -> acc @ [x |> padTo32BytesRight]
        | x when x.Length > 64 -> 
            let _x = x[..63]
            let x = x[64..]
            wrapBytesAcrossWords x (acc @ [_x])
        | _ -> [""]

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ABI Handling
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    let Uint8 value = (Bitness.B8, value) |> Uint
    let Uint16 value = (Bitness.B16, value) |> Uint
    let Uint24 value = (Bitness.B24, value) |> Uint
    let Uint32 value = (Bitness.B32, value) |> Uint
    let Uint40 value = (Bitness.B40, value) |> Uint
    let Uint48 value = (Bitness.B48, value) |> Uint
    let Uint56 value = (Bitness.B56, value) |> Uint
    let Uint64 value = (Bitness.B64, value) |> Uint
    let Uint72 value = (Bitness.B72, value) |> Uint
    let Uint80 value = (Bitness.B80, value) |> Uint
    let Uint88 value = (Bitness.B88, value) |> Uint
    let Uint96 value = (Bitness.B96, value) |> Uint
    let Uint104 value = (Bitness.B104, value) |> Uint
    let Uint112 value = (Bitness.B112, value) |> Uint
    let Uint120 value = (Bitness.B120, value) |> Uint
    let Uint128 value = (Bitness.B128, value) |> Uint
    let Uint136 value = (Bitness.B136, value) |> Uint
    let Uint144 value = (Bitness.B144, value) |> Uint
    let Uint152 value = (Bitness.B152, value) |> Uint
    let Uint160 value = (Bitness.B160, value) |> Uint
    let Uint168 value = (Bitness.B168, value) |> Uint
    let Uint176 value = (Bitness.B176, value) |> Uint
    let Uint184 value = (Bitness.B184, value) |> Uint
    let Uint192 value = (Bitness.B192, value) |> Uint
    let Uint200 value = (Bitness.B200, value) |> Uint
    let Uint208 value = (Bitness.B208, value) |> Uint
    let Uint216 value = (Bitness.B216, value) |> Uint
    let Uint224 value = (Bitness.B224, value) |> Uint
    let Uint232 value = (Bitness.B232, value) |> Uint
    let Uint240 value = (Bitness.B240, value) |> Uint
    let Uint248 value = (Bitness.B248, value) |> Uint
    let Uint256 value = (Bitness.B256, value) |> Uint
    let Uint8ArraySz value = (Bitness.B8, value) |> UintArraySz
    let Uint16ArraySz value = (Bitness.B16, value) |> UintArraySz
    let Uint24ArraySz value = (Bitness.B24, value) |> UintArraySz
    let Uint32ArraySz value = (Bitness.B32, value) |> UintArraySz
    let Uint40ArraySz value = (Bitness.B40, value) |> UintArraySz
    let Uint48ArraySz value = (Bitness.B48, value) |> UintArraySz
    let Uint56ArraySz value = (Bitness.B56, value) |> UintArraySz
    let Uint64ArraySz value = (Bitness.B64, value) |> UintArraySz
    let Uint72ArraySz value = (Bitness.B72, value) |> UintArraySz
    let Uint80ArraySz value = (Bitness.B80, value) |> UintArraySz
    let Uint88ArraySz value = (Bitness.B88, value) |> UintArraySz
    let Uint96ArraySz value = (Bitness.B96, value) |> UintArraySz
    let Uint104ArraySz value = (Bitness.B104, value) |> UintArraySz
    let Uint112ArraySz value = (Bitness.B112, value) |> UintArraySz
    let Uint120ArraySz value = (Bitness.B120, value) |> UintArraySz
    let Uint128ArraySz value = (Bitness.B128, value) |> UintArraySz
    let Uint136ArraySz value = (Bitness.B136, value) |> UintArraySz
    let Uint144ArraySz value = (Bitness.B144, value) |> UintArraySz
    let Uint152ArraySz value = (Bitness.B152, value) |> UintArraySz
    let Uint160ArraySz value = (Bitness.B160, value) |> UintArraySz
    let Uint168ArraySz value = (Bitness.B168, value) |> UintArraySz
    let Uint176ArraySz value = (Bitness.B176, value) |> UintArraySz
    let Uint184ArraySz value = (Bitness.B184, value) |> UintArraySz
    let Uint192ArraySz value = (Bitness.B192, value) |> UintArraySz
    let Uint200ArraySz value = (Bitness.B200, value) |> UintArraySz
    let Uint208ArraySz value = (Bitness.B208, value) |> UintArraySz
    let Uint216ArraySz value = (Bitness.B216, value) |> UintArraySz
    let Uint224ArraySz value = (Bitness.B224, value) |> UintArraySz
    let Uint232ArraySz value = (Bitness.B232, value) |> UintArraySz
    let Uint240ArraySz value = (Bitness.B240, value) |> UintArraySz
    let Uint248ArraySz value = (Bitness.B248, value) |> UintArraySz
    let Uint256ArraySz value = (Bitness.B256, value) |> UintArraySz
    let Uint8Array value = (Bitness.B8, value) |> UintArray
    let Uint16Array value = (Bitness.B16, value) |> UintArray
    let Uint24Array value = (Bitness.B24, value) |> UintArray
    let Uint32Array value = (Bitness.B32, value) |> UintArray
    let Uint40Array value = (Bitness.B40, value) |> UintArray
    let Uint48Array value = (Bitness.B48, value) |> UintArray
    let Uint56Array value = (Bitness.B56, value) |> UintArray
    let Uint64Array value = (Bitness.B64, value) |> UintArray
    let Uint72Array value = (Bitness.B72, value) |> UintArray
    let Uint80Array value = (Bitness.B80, value) |> UintArray
    let Uint88Array value = (Bitness.B88, value) |> UintArray
    let Uint96Array value = (Bitness.B96, value) |> UintArray
    let Uint104Array value = (Bitness.B104, value) |> UintArray
    let Uint112Array value = (Bitness.B112, value) |> UintArray
    let Uint120Array value = (Bitness.B120, value) |> UintArray
    let Uint128Array value = (Bitness.B128, value) |> UintArray
    let Uint136Array value = (Bitness.B136, value) |> UintArray
    let Uint144Array value = (Bitness.B144, value) |> UintArray
    let Uint152Array value = (Bitness.B152, value) |> UintArray
    let Uint160Array value = (Bitness.B160, value) |> UintArray
    let Uint168Array value = (Bitness.B168, value) |> UintArray
    let Uint176Array value = (Bitness.B176, value) |> UintArray
    let Uint184Array value = (Bitness.B184, value) |> UintArray
    let Uint192Array value = (Bitness.B192, value) |> UintArray
    let Uint200Array value = (Bitness.B200, value) |> UintArray
    let Uint208Array value = (Bitness.B208, value) |> UintArray
    let Uint216Array value = (Bitness.B216, value) |> UintArray
    let Uint224Array value = (Bitness.B224, value) |> UintArray
    let Uint232Array value = (Bitness.B232, value) |> UintArray
    let Uint240Array value = (Bitness.B240, value) |> UintArray
    let Uint248Array value = (Bitness.B248, value) |> UintArray
    let Uint256Array value = (Bitness.B256, value) |> UintArray
    
    let Int8 value = (Bitness.B8, value) |> Int
    let Int16 value = (Bitness.B16, value) |> Int
    let Int24 value = (Bitness.B24, value) |> Int
    let Int32 value = (Bitness.B32, value) |> Int
    let Int40 value = (Bitness.B40, value) |> Int
    let Int48 value = (Bitness.B48, value) |> Int
    let Int56 value = (Bitness.B56, value) |> Int
    let Int64 value = (Bitness.B64, value) |> Int
    let Int72 value = (Bitness.B72, value) |> Int
    let Int80 value = (Bitness.B80, value) |> Int
    let Int88 value = (Bitness.B88, value) |> Int
    let Int96 value = (Bitness.B96, value) |> Int
    let Int104 value = (Bitness.B104, value) |> Int
    let Int112 value = (Bitness.B112, value) |> Int
    let Int120 value = (Bitness.B120, value) |> Int
    let Int128 value = (Bitness.B128, value) |> Int
    let Int136 value = (Bitness.B136, value) |> Int
    let Int144 value = (Bitness.B144, value) |> Int
    let Int152 value = (Bitness.B152, value) |> Int
    let Int160 value = (Bitness.B160, value) |> Int
    let Int168 value = (Bitness.B168, value) |> Int
    let Int176 value = (Bitness.B176, value) |> Int
    let Int184 value = (Bitness.B184, value) |> Int
    let Int192 value = (Bitness.B192, value) |> Int
    let Int200 value = (Bitness.B200, value) |> Int
    let Int208 value = (Bitness.B208, value) |> Int
    let Int216 value = (Bitness.B216, value) |> Int
    let Int224 value = (Bitness.B224, value) |> Int
    let Int232 value = (Bitness.B232, value) |> Int
    let Int240 value = (Bitness.B240, value) |> Int
    let Int248 value = (Bitness.B248, value) |> Int
    let Int256 value = (Bitness.B256, value) |> Int
    let Int8ArraySz value = (Bitness.B8, value) |> IntArraySz
    let Int16ArraySz value = (Bitness.B16, value) |> IntArraySz
    let Int24ArraySz value = (Bitness.B24, value) |> IntArraySz
    let Int32ArraySz value = (Bitness.B32, value) |> IntArraySz
    let Int40ArraySz value = (Bitness.B40, value) |> IntArraySz
    let Int48ArraySz value = (Bitness.B48, value) |> IntArraySz
    let Int56ArraySz value = (Bitness.B56, value) |> IntArraySz
    let Int64ArraySz value = (Bitness.B64, value) |> IntArraySz
    let Int72ArraySz value = (Bitness.B72, value) |> IntArraySz
    let Int80ArraySz value = (Bitness.B80, value) |> IntArraySz
    let Int88ArraySz value = (Bitness.B88, value) |> IntArraySz
    let Int96ArraySz value = (Bitness.B96, value) |> IntArraySz
    let Int104ArraySz value = (Bitness.B104, value) |> IntArraySz
    let Int112ArraySz value = (Bitness.B112, value) |> IntArraySz
    let Int120ArraySz value = (Bitness.B120, value) |> IntArraySz
    let Int128ArraySz value = (Bitness.B128, value) |> IntArraySz
    let Int136ArraySz value = (Bitness.B136, value) |> IntArraySz
    let Int144ArraySz value = (Bitness.B144, value) |> IntArraySz
    let Int152ArraySz value = (Bitness.B152, value) |> IntArraySz
    let Int160ArraySz value = (Bitness.B160, value) |> IntArraySz
    let Int168ArraySz value = (Bitness.B168, value) |> IntArraySz
    let Int176ArraySz value = (Bitness.B176, value) |> IntArraySz
    let Int184ArraySz value = (Bitness.B184, value) |> IntArraySz
    let Int192ArraySz value = (Bitness.B192, value) |> IntArraySz
    let Int200ArraySz value = (Bitness.B200, value) |> IntArraySz
    let Int208ArraySz value = (Bitness.B208, value) |> IntArraySz
    let Int216ArraySz value = (Bitness.B216, value) |> IntArraySz
    let Int224ArraySz value = (Bitness.B224, value) |> IntArraySz
    let Int232ArraySz value = (Bitness.B232, value) |> IntArraySz
    let Int240ArraySz value = (Bitness.B240, value) |> IntArraySz
    let Int248ArraySz value = (Bitness.B248, value) |> IntArraySz
    let Int256ArraySz value = (Bitness.B256, value) |> IntArraySz
    let Int8Array value = (Bitness.B8, value) |> IntArray
    let Int16Array value = (Bitness.B16, value) |> IntArray
    let Int24Array value = (Bitness.B24, value) |> IntArray
    let Int32Array value = (Bitness.B32, value) |> IntArray
    let Int40Array value = (Bitness.B40, value) |> IntArray
    let Int48Array value = (Bitness.B48, value) |> IntArray
    let Int56Array value = (Bitness.B56, value) |> IntArray
    let Int64Array value = (Bitness.B64, value) |> IntArray
    let Int72Array value = (Bitness.B72, value) |> IntArray
    let Int80Array value = (Bitness.B80, value) |> IntArray
    let Int88Array value = (Bitness.B88, value) |> IntArray
    let Int96Array value = (Bitness.B96, value) |> IntArray
    let Int104Array value = (Bitness.B104, value) |> IntArray
    let Int112Array value = (Bitness.B112, value) |> IntArray
    let Int120Array value = (Bitness.B120, value) |> IntArray
    let Int128Array value = (Bitness.B128, value) |> IntArray
    let Int136Array value = (Bitness.B136, value) |> IntArray
    let Int144Array value = (Bitness.B144, value) |> IntArray
    let Int152Array value = (Bitness.B152, value) |> IntArray
    let Int160Array value = (Bitness.B160, value) |> IntArray
    let Int168Array value = (Bitness.B168, value) |> IntArray
    let Int176Array value = (Bitness.B176, value) |> IntArray
    let Int184Array value = (Bitness.B184, value) |> IntArray
    let Int192Array value = (Bitness.B192, value) |> IntArray
    let Int200Array value = (Bitness.B200, value) |> IntArray
    let Int208Array value = (Bitness.B208, value) |> IntArray
    let Int216Array value = (Bitness.B216, value) |> IntArray
    let Int224Array value = (Bitness.B224, value) |> IntArray
    let Int232Array value = (Bitness.B232, value) |> IntArray
    let Int240Array value = (Bitness.B240, value) |> IntArray
    let Int248Array value = (Bitness.B248, value) |> IntArray
    let Int256Array value = (Bitness.B256, value) |> IntArray
    
    let Byte1 value = (L1, value) |> BytesN
    let Byte2 value = (L2, value) |> BytesN
    let Byte3 value = (L3, value) |> BytesN
    let Byte4 value = (L4, value) |> BytesN
    let Byte5 value = (L5, value) |> BytesN
    let Byte6 value = (L6, value) |> BytesN
    let Byte7 value = (L7, value) |> BytesN
    let Byte8 value = (L8, value) |> BytesN
    let Byte9 value = (L9, value) |> BytesN
    let Byte10 value = (L10, value) |> BytesN
    let Byte11 value = (L11, value) |> BytesN
    let Byte12 value = (L12, value) |> BytesN
    let Byte13 value = (L13, value) |> BytesN
    let Byte14 value = (L14, value) |> BytesN
    let Byte15 value = (L15, value) |> BytesN
    let Byte16 value = (L16, value) |> BytesN
    let Byte17 value = (L17, value) |> BytesN
    let Byte18 value = (L18, value) |> BytesN
    let Byte19 value = (L19, value) |> BytesN
    let Byte20 value = (L20, value) |> BytesN
    let Byte21 value = (L21, value) |> BytesN
    let Byte22 value = (L22, value) |> BytesN
    let Byte23 value = (L23, value) |> BytesN
    let Byte24 value = (L24, value) |> BytesN
    let Byte25 value = (L25, value) |> BytesN
    let Byte26 value = (L26, value) |> BytesN
    let Byte27 value = (L27, value) |> BytesN
    let Byte28 value = (L28, value) |> BytesN
    let Byte29 value = (L29, value) |> BytesN
    let Byte30 value = (L30, value) |> BytesN
    let Byte31 value = (L31, value) |> BytesN
    let Byte32 value = (L32, value) |> BytesN

    let Byte1ArraySz value = (L1, value) |> BytesNArraySz
    let Byte2ArraySz value = (L2, value) |> BytesNArraySz
    let Byte3ArraySz value = (L3, value) |> BytesNArraySz
    let Byte4ArraySz value = (L4, value) |> BytesNArraySz
    let Byte5ArraySz value = (L5, value) |> BytesNArraySz
    let Byte6ArraySz value = (L6, value) |> BytesNArraySz
    let Byte7ArraySz value = (L7, value) |> BytesNArraySz
    let Byte8ArraySz value = (L8, value) |> BytesNArraySz
    let Byte9ArraySz value = (L9, value) |> BytesNArraySz
    let Byte10ArraySz value = (L10, value) |> BytesNArraySz
    let Byte11ArraySz value = (L11, value) |> BytesNArraySz
    let Byte12ArraySz value = (L12, value) |> BytesNArraySz
    let Byte13ArraySz value = (L13, value) |> BytesNArraySz
    let Byte14ArraySz value = (L14, value) |> BytesNArraySz
    let Byte15ArraySz value = (L15, value) |> BytesNArraySz
    let Byte16ArraySz value = (L16, value) |> BytesNArraySz
    let Byte17ArraySz value = (L17, value) |> BytesNArraySz
    let Byte18ArraySz value = (L18, value) |> BytesNArraySz
    let Byte19ArraySz value = (L19, value) |> BytesNArraySz
    let Byte20ArraySz value = (L20, value) |> BytesNArraySz
    let Byte21ArraySz value = (L21, value) |> BytesNArraySz
    let Byte22ArraySz value = (L22, value) |> BytesNArraySz
    let Byte23ArraySz value = (L23, value) |> BytesNArraySz
    let Byte24ArraySz value = (L24, value) |> BytesNArraySz
    let Byte25ArraySz value = (L25, value) |> BytesNArraySz
    let Byte26ArraySz value = (L26, value) |> BytesNArraySz
    let Byte27ArraySz value = (L27, value) |> BytesNArraySz
    let Byte28ArraySz value = (L28, value) |> BytesNArraySz
    let Byte29ArraySz value = (L29, value) |> BytesNArraySz
    let Byte30ArraySz value = (L30, value) |> BytesNArraySz
    let Byte31ArraySz value = (L31, value) |> BytesNArraySz
    let Byte32ArraySz value = (L32, value) |> BytesNArraySz
    
    let Byte1Array value = (L1, value) |> BytesNArray
    let Byte2Array value = (L2, value) |> BytesNArray
    let Byte3Array value = (L3, value) |> BytesNArray
    let Byte4Array value = (L4, value) |> BytesNArray
    let Byte5Array value = (L5, value) |> BytesNArray
    let Byte6Array value = (L6, value) |> BytesNArray
    let Byte7Array value = (L7, value) |> BytesNArray
    let Byte8Array value = (L8, value) |> BytesNArray
    let Byte9Array value = (L9, value) |> BytesNArray
    let Byte10Array value = (L10, value) |> BytesNArray
    let Byte11Array value = (L11, value) |> BytesNArray
    let Byte12Array value = (L12, value) |> BytesNArray
    let Byte13Array value = (L13, value) |> BytesNArray
    let Byte14Array value = (L14, value) |> BytesNArray
    let Byte15Array value = (L15, value) |> BytesNArray
    let Byte16Array value = (L16, value) |> BytesNArray
    let Byte17Array value = (L17, value) |> BytesNArray
    let Byte18Array value = (L18, value) |> BytesNArray
    let Byte19Array value = (L19, value) |> BytesNArray
    let Byte20Array value = (L20, value) |> BytesNArray
    let Byte21Array value = (L21, value) |> BytesNArray
    let Byte22Array value = (L22, value) |> BytesNArray
    let Byte23Array value = (L23, value) |> BytesNArray
    let Byte24Array value = (L24, value) |> BytesNArray
    let Byte25Array value = (L25, value) |> BytesNArray
    let Byte26Array value = (L26, value) |> BytesNArray
    let Byte27Array value = (L27, value) |> BytesNArray
    let Byte28Array value = (L28, value) |> BytesNArray
    let Byte29Array value = (L29, value) |> BytesNArray
    let Byte30Array value = (L30, value) |> BytesNArray
    let Byte31Array value = (L31, value) |> BytesNArray
    let Byte32Array value = (L32, value) |> BytesNArray
    
        
    let bitnessToInt bitness =
        match bitness with
        | B8 -> 8
        | B16 -> 16
        | B24 -> 24  
        | B32 -> 32
        | B40 -> 40
        | B48 -> 48
        | B56 -> 56
        | B64 -> 64
        | B72 -> 72
        | B80 -> 80
        | B88 -> 88
        | B96 -> 96
        | B104 -> 104
        | B112 -> 112 
        | B120 -> 120
        | B128 -> 128
        | B136 -> 136
        | B144 -> 144
        | B152 -> 152
        | B160 -> 160
        | B168 -> 168
        | B176 -> 176
        | B184 -> 184
        | B192 -> 192
        | B200 -> 200
        | B208 -> 208
        | B216 -> 216
        | B224 -> 224
        | B232 -> 232
        | B240 -> 240
        | B248 -> 248
        | B256 -> 256
        
    let byteLengthToInt length =
        match length with
        | L1 -> 1
        | L2 -> 2
        | L3 -> 3
        | L4 -> 4
        | L5 -> 5
        | L6 -> 6
        | L7 -> 7
        | L8 -> 8
        | L9 -> 9
        | L10 -> 10 
        | L11 -> 11
        | L12 -> 12
        | L13 -> 13
        | L14 -> 14
        | L15 -> 15
        | L16 -> 16
        | L17 -> 17
        | L18 -> 18
        | L19 -> 19
        | L20 -> 20
        | L21 -> 21
        | L22 -> 22
        | L23 -> 23
        | L24 -> 24
        | L25 -> 25
        | L26 -> 26
        | L27 -> 27
        | L28 -> 28
        | L29 -> 29
        | L30 -> 30 
        | L31 -> 31
        | L32 -> 32
        
    
    ///
    /// Returns the proper count of items in a tuple, taking into account the
    /// contents of sized arrays, which consume several 'slots' directly without
    /// offset or counter. Strings and Bytes need special compensation in the
    /// calculation, as their space requirements are variable.
    /// 
    let private countOfArguments (evmDatatypeList: EVMDatatype list) =
        let rec countLoop (evmDatatypeList: EVMDatatype list) acc =
            match evmDatatypeList with
            | head :: tail ->
                match head with
                | BytesNArraySz (_, bArr) -> countLoop tail (acc + bArr.Length)
                | AddressArraySz arr -> countLoop tail (acc + arr.Length)
                | UintArraySz (_, uArr) -> countLoop tail (acc + uArr.Length)
                | IntArraySz (_, iArr) ->  countLoop tail (acc + iArr.Length)
                | BoolArraySz bArr -> countLoop tail (acc + bArr.Length)
                | _ -> countLoop tail (acc + 1)
            | [] -> acc
        countLoop evmDatatypeList 0
    
    
    ///
    /// Returns a formatted bytecode string for the 'data' argument in a
    /// EthParam1559Call, or other related transaction functions. The
    /// EVMDatatype list contains typed arguments for processing recursively,
    /// formatted in a nested way similar to how it is specified in Remix or
    /// other EVM development environments. 
    /// 
    /// **Warning** Double arrays `[][]` are not supported.
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
        // in the argument string, and the contents are only appended after all
        // other simple values in the current tuple are exhausted. The EVM will
        // look for the dynamic contents at the offset value, where it expects
        // to find a count of the members of the dynamic type (a true count in
        // most cases except `bytes`, which are a count of the hex bytes
        // instead).
        //
        // In the simple case, this code will format the datatype according
        // to the rules, and place it in order as it was encountered. In sized 
        // array cases, the logic is much the same, repeated for all members of
        // the array. 
        //
        // Due to their extra complication, dynamic types carry the notion of
        // the 'cursor.' This is a simple abstraction that tracks the next
        // available 'slot' that a blob of data may be placed. The cursor is
        // initialized beyond the end of the array, which looks strange but is
        // the desired behavior, as this is the first 'slot' to place a dynamic
        // type's contents.
        //
        // The code then proceeds to grab the current cursor's value and place
        // it into a EVM word, and then it goes about building the 'blob' that
        // contains the real values. This is appended as more work on the
        // current `tail`. The cursor value is then updated to reflect the size
        // of the blob. Only types that insert content at an offset location
        // need to manipulate the cursor value, usually by an accounting of
        // their size, plus 1 for their own internal count representation.
        //
        // Blob values are finally appended to the accumulator at the end of the 
        // current tuple. This is important; the computed offsets to a given
        // dynamic type are relative to the beginning of the current tuple!
        //
        // Overall, this is your standard FSharp recursive function, bent into
        // an awkward shape due to the requirements of the output. 
        //
        // An initial version of this code incorrectly initialized the cursor to
        // the length of the input list, which corresponded to the count of top-
        // level items. This was incorrect, as it failed to account for sized
        // array types (which consume as many words as they have members) as
        // well as `Bytes` and `String`, which are variable based on how many
        // words they consume when wrapping is factored in. The top level
        // (implied) tuple, as well as all nested `Tuple` types use this
        // calculation.
    
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //// Set of internal functions
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
        let unsignedBoundsCheck power value=
            bigint.Parse(value) |> fun i -> not(i >= bigint.Pow(2, power)) && not(i < 0I)
            
        
        // Inverts the sense of the test because we action on true down in the recursive
        let arrayUnsignedFalseCheck values power =
            match values |> List.map (unsignedBoundsCheck power) |> List.contains false with
            | true -> false
            | false -> true
            
            
        let signedBoundsCheck power value =
            bigint.Parse(value) |> fun i -> not(i >= bigint.Pow(2, power)) && not(i < -(bigint.Pow(2, power)))
            
        
        let arraySignedFalseCheck values power =
            match values |> List.map (signedBoundsCheck power) |> List.contains false with
            | true -> false
            | false -> true
            
           
        let bytesBoundsCheck size value =
            value |> strip0x |> fun s -> s.Length <= size * 2
            
            
        let arrayBytesBoundsCheck (values: string list) size =
            match values |> List.map (bytesBoundsCheck size) |> List.contains false with
            | true -> false
            | false -> true


        let addressLengthCheck (value: string) =
            value |> strip0x |> fun v -> v.Length = 40
            
            
        let arrayAddressLengthCheck (values: string list) =
            match values |> List.map addressLengthCheck |> List.contains false with
            | true -> false
            | false -> true
        
        
        let bytesTwosCheck (value: string) =
            value |> strip0x |> fun v -> v.Length % 2 = 0
            
            
        let cursor = countOfArguments evmDatatypeList
        
                
        let rec unpackInputAndProcess list acc cursor =
            match list with
            | head :: tail ->
                match head with
                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                //// Address Types
                ///////////////////////////////////////////////////////////////////////////////////////////////////////                
                
                
                | Address a ->
                    if addressLengthCheck a then
                        unpackInputAndProcess tail (acc + $"{a |> strip0x |> padTo32BytesLeft }") cursor
                    else $"#Specified Address {a} length incorrect#"
                | AddressArraySz arr ->
                    if arrayAddressLengthCheck arr then        
                        let payload =
                            arr
                            |> List.map(strip0x >> fun p -> $"{padTo32BytesLeft p}")
                            |> String.concat ""
                        unpackInputAndProcess tail (acc + payload) cursor
                    else $"#Specified Address array contains address with incorrect length: \n{arr}#"                        
                | AddressArray arr ->
                    if arrayAddressLengthCheck arr then    
                        let acc = acc + returnCurrentOffset cursor
                        let payload =
                            arr
                            |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesLeft}") (returnCountOfItems arr)
                            |> Blob
                        let tail = tail @ [payload]
                        unpackInputAndProcess tail acc (cursor + arr.Length + 1)
                    else $"#Specified Address array contains address with incorrect length: \n{arr}#"
                
                
                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                //// Unsigned Integer Types
                ///////////////////////////////////////////////////////////////////////////////////////////////////////

                
                | Uint (bitness, u) ->
                    let bits = bitnessToInt bitness
                    if unsignedBoundsCheck bits u then
                        unpackInputAndProcess tail (acc + $"{formatTypesInt u}") cursor
                    else $"#Value {u} was larger than unsigned {bits} bit integer allows#"
                | UintArraySz (bitness, uArr) ->
                    let bits = bitnessToInt bitness
                    if arrayUnsignedFalseCheck uArr bits then
                        let payload =
                            uArr
                            |> List.map(fun p -> $"{p |> formatTypesInt}")
                            |> String.concat ""
                        unpackInputAndProcess tail (acc + payload) cursor
                    else $"#Array contained a value larger than unsigned {bits} bit integer allows:\n{uArr}#"
                | UintArray (bitness, uArr) ->
                    let bits = bitnessToInt bitness
                    if arrayUnsignedFalseCheck uArr bits then
                        let acc = acc + returnCurrentOffset cursor
                        let payload =
                            uArr
                            |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt}") (returnCountOfItems uArr)
                            |> Blob
                        let tail = tail @ [payload]
                        unpackInputAndProcess tail acc (cursor + uArr.Length + 1)
                    else $"#Array contained a value larger than unsigned {bits} bit integer allows:\n{uArr}#"
                

                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                //// Signed Integer Types
                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                | Int (bitness, i) ->
                    let bits = bitnessToInt bitness
                    if signedBoundsCheck (bits - 1) i then    
                        unpackInputAndProcess tail (acc + $"{formatTypesInt i}") cursor
                    else $"#Value {i} was larger than signed 256 bit integer allows#"
                | IntArraySz (bitness, iArr) ->
                    let bits = bitnessToInt bitness
                    if arraySignedFalseCheck iArr bits then
                        let payload =
                            iArr
                            |> List.map(fun p -> $"{p |> formatTypesInt }")
                            |> String.concat ""
                        unpackInputAndProcess tail (acc + payload) cursor
                    else $"#Array {iArr} contained a value larger than unsigned {bits} bit integer allows#"
                | IntArray (bitness, iArr) ->
                    let bits = bitnessToInt bitness
                    if arraySignedFalseCheck iArr bits then
                        let acc = acc + returnCurrentOffset cursor
                        let payload =
                            iArr
                            |> List.fold (fun acc s -> $"{acc}{s |> formatTypesInt }") (returnCountOfItems iArr)
                            |> Blob 
                        let tail = tail @ [payload]
                        unpackInputAndProcess tail acc (cursor + iArr.Length + 1)
                    else $"#Array {iArr} contained a value larger than unsigned {bits} bit integer allows#"

                
                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                //// Bool Types
                ///////////////////////////////////////////////////////////////////////////////////////////////////////                
                
                
                | Bool b -> unpackInputAndProcess tail (acc + convertBoolToInt b) cursor
                | BoolArraySz bArr ->
                    let payload = bArr |> List.map convertBoolToInt |> String.concat ""
                    unpackInputAndProcess tail (acc + payload) cursor
                | BoolArray bArr -> 
                    let acc = acc + returnCurrentOffset cursor
                    let payload =
                        bArr
                        |> List.fold (fun acc s -> $"{acc}{convertBoolToInt s}") (returnCountOfItems bArr)
                        |> Blob 
                    let tail = tail @ [payload]
                    unpackInputAndProcess tail acc (cursor + bArr.Length + 1)


                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                //// Sized Byte Types
                ///////////////////////////////////////////////////////////////////////////////////////////////////////                                                
                
                
                | BytesN (length, b) ->
                    let _len = byteLengthToInt length
                    if bytesBoundsCheck _len b then
                        unpackInputAndProcess tail (acc + $"{b |> strip0x |> padTo32BytesRight}") cursor
                    else $"#Byte value {b} is larger than indicated size {_len}#"
                | BytesNArraySz (length, bs) ->
                    let _len = byteLengthToInt length
                    if arrayBytesBoundsCheck bs _len then
                        let payload =
                            bs
                            |> List.map(strip0x >> fun p -> $"{padTo32BytesRight p}")
                            |> String.concat ""
                        unpackInputAndProcess tail (acc + payload) cursor
                    else $"#Byte value in {bs} is larger than indicated size {_len}#"
                | BytesNArray (length, bs) ->
                    let _len = byteLengthToInt length
                    if arrayBytesBoundsCheck bs _len then
                        let _len = byteLengthToInt length
                        let acc = acc + returnCurrentOffset cursor
                        let payload =
                            bs
                            |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesRight}") (returnCountOfItems bs)
                            |> Blob 
                        let tail = tail @ [payload]
                        unpackInputAndProcess tail acc (cursor + bs.Length + 1 )
                    else $"#Byte value in {bs} is larger than indicated size {_len}#"
                
                
                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                //// Dynamic Byte Types
                ///////////////////////////////////////////////////////////////////////////////////////////////////////                
                
                
                | Bytes bs ->
                    if bytesTwosCheck bs then    
                        let acc = acc + returnCurrentOffset cursor
                        let bs = bs |> strip0x
                        let contents =
                            bs.Length
                            |> byteDivide2
                            |> formatTypes padTo32BytesLeft
                            |> fun s -> s + (wrapBytesAcrossWords bs [] |> String.concat "")
                        let tail = tail @ [ contents |> Blob ]
                        unpackInputAndProcess tail acc (cursor + (contents.Length / 64))
                    else $"#Bytes {bs} must be composed of pairs of characters. Length uneven.#"
                | BytesArraySz bsArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents = (unpackInputAndProcess bsArr "" (countOfArguments bsArr))
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64 ))
                | BytesArray bsArr ->
                    let acc = acc + returnCurrentOffset cursor
                    let contents =
                        returnCountOfItems bsArr
                        |> fun s -> s  + (unpackInputAndProcess bsArr "" bsArr.Length)
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
                    let contents =
                        returnCountOfItems sArr
                        |> fun s -> s + (unpackInputAndProcess sArr "" sArr.Length)
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64) )
                
                
                ///////////////////////////////////////////////////////////////////////////////////////////////////////
                //// Tuple Types
                ///////////////////////////////////////////////////////////////////////////////////////////////////////                
                
                
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
                    let contents =
                        returnCountOfItems tArr
                        |> fun s -> s + (unpackInputAndProcess tArr "" (countOfArguments tArr))
                    let tail = tail @ [ contents |> Blob ]
                    unpackInputAndProcess tail acc (cursor + (contents.Length / 64)) 
                
                | Blob blob -> unpackInputAndProcess tail (acc + blob) cursor
//                | Function f -> unpackInputAndProcess tail (acc + $"{f |> strip0x |> padTo32BytesRight }") cursor
//                
//                | FunctionArraySz fArr ->
//                    unpackInputAndProcess tail (acc + (fArr |> List.map(strip0x >> fun p -> $"{padTo32BytesRight p}") |> String.concat "")) cursor
//                
//                | FunctionArray fArr -> 
//                    let acc = acc + returnCurrentOffset cursor
//                    let tail = tail @ [ fArr |> List.fold (fun acc s -> $"{acc}{s |> strip0x |> padTo32BytesRight}") (returnCountOfItems fArr) |> Blob ]
//                    unpackInputAndProcess tail acc (cursor + fArr.Length + 1)
            | [] -> acc

        let bytestring = unpackInputAndProcess evmDatatypeList "" cursor
        
        if bytestring.Contains('#') then
            let matcher = RegularExpressions.Regex("(#.*?)#")
            let reason = matcher.Match bytestring |> fun s -> s.Value.Replace("#", String.Empty)
            $"Arguments to EVM function failed validation: {reason}"
            |> FunctionArgumentsValidationError
            |> Error
        else bytestring |> Ok


    ///
    /// Returns a substring from a given offset, preset to one EVM word.
    let private emitSubstring start (blob: string) =
        blob.Substring(start, 64)
        
        
    ///
    /// Returns the substring with a hex specifier prepended. 
    let private emitSubstringPrepend0x start blob =
        emitSubstring start blob
        |> fun s ->
            match s with
            | x when x = zeroEVMValue -> nullAddress |> prepend0x
            | x -> x.Remove(0,24) |> prepend0x


    ///
    /// Returns the substring specifically for sized bytes, with a hex specifier
    /// prepended. Until I get around to implementing a 'proper' type system for
    /// this, I can't trim 0's, because a set of trailing 0's might be intended,
    /// or unintended.
    let private emitSubstringPrepend0xBytes start blob =
        emitSubstring start blob
        |> fun s ->
            match s with
            | x when x = zeroEVMValue -> "0" |> prepend0x
            | x -> x
        |> prepend0x 
        
        
    ///
    /// Returns an integer value contained in a given substring. This assumes
    /// that the count/offset value being interpreted is not larger than a
    /// signed int. `Int32` is used because `List.init` (where this value is
    /// being used) only accepts this type.
    /// 
    let private emitSubstringAsInt start blob =
        emitSubstring start blob |> fun i -> Convert.ToInt32(i, 16)


    ///
    /// Returns an integer value contained in a given substring, with a
    /// byte-length-to-char-length compensator applied. `Int32` is used because
    /// functions consuming this value assume `int`.
    /// 
    let private emitSubstringAsOffset blob start =
        emitSubstring start blob |> fun i -> Convert.ToInt32(i, 16) |> fun i -> i * 2


    ///
    /// Returns a boolean value after interpreting a substring in a boolean
    /// context. Non-0 values are taken as `true`.
    /// 
    let private emitSubstringAsBool start blob =
        emitSubstring start blob
        |> function
            | "0000000000000000000000000000000000000000000000000000000000000000" -> false
            | _ -> true


    ///
    /// Returns a bigint that has been converted to a string, based upon the
    /// value contained in a substring.
    /// 
    let private emitSubstringAsConvertedStringUnsigned start blob =
        emitSubstring start blob |> hexToBigintUnsigned |> fun s -> s.ToString()
        
        
    ///
    /// Returns a bigint that has been converted to a string, based upon the
    /// value contained in a substring.
    /// 
    let private emitSubstringAsConvertedStringSigned start blob =
        emitSubstring start blob |> hexToBigInt |> fun s -> s.ToString()


    ///
    /// Returns the internal typed representation of an EVM function return
    /// value. This is typically the result of an `EthParam1559Call`. When
    /// calling a contract's function from inside web3.fs or elsewhere, the RPC
    /// response contains a value that is formatted according to the signature
    /// of the return type(s).
    /// 
    /// **Warning** The numeric types are represented in web3.fs as the widest
    /// version of their type, and the returned types will thus all be `__256`.
    /// However, the values contained therein can be trusted to be conforming to
    /// their intended size.
    /// **Warning** Multidimensional arrays and the `Function` types aren't
    /// supported.
    ///  
    let internal createOutputEvmTypes (evmList: EVMDatatype list) (evmOutput: string) =
            
        // The general form of these cases is to take in the current position of
        // the work in the `evmOutput` string, derive a floating 'offset' value
        // from it, and then to unpack/manipulate the contents of the string as
        // needed for each data type's context.
        //
        // The sized array types have additional math to account for their
        // contents, because unlike the dynamic array types, they don't leave an
        // offset at which their contents are stored, but instead the contents
        // are placed directly at the beginning cursor position. Thus they must
        // advance the cursor more than one 'word'.
        //
        // Bytes (and by extension String) types are even more involved, since
        // they can wrap 'words' and thus extracting their contents is more
        // complex. The Bytes array types also function with a faked offset
        // value, placed so that the Bytes handler works properly.
        
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
                
                | Int _ ->
                    let pointer = cursor * 64
                    let acc = acc @ [(Int256 $"{emitSubstringAsConvertedStringSigned pointer evmOutput}")]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | IntArraySz (_, iArr) ->
                    let mutable offset = (cursor * 64)
                    let contents = List.init iArr.Length (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedStringSigned offset evmOutput}")
                    let acc = acc @ [Int256ArraySz contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + iArr.Length)
                
                | IntArray _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = emitSubstringAsInt offset evmOutput
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedStringSigned offset evmOutput}")
                    let acc = acc @ [Int256Array contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | Uint _ ->
                    let pointer = cursor * 64
                    let acc = acc @ [(Uint256 $"{emitSubstringAsConvertedStringUnsigned pointer evmOutput}")]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | UintArraySz (_, uArr) ->
                    let mutable offset = (cursor * 64)
                    let contents = List.init uArr.Length (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedStringUnsigned offset evmOutput}")
                    let acc = acc @ [Uint256ArraySz contents]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | UintArray _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = emitSubstringAsInt offset evmOutput
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringAsConvertedStringUnsigned offset evmOutput}")
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
                
                | BytesN _ ->
                    let pointer = cursor * 64
                    let acc = acc @ [BytesN (L32, $"{emitSubstringPrepend0xBytes pointer evmOutput}")]
                    unpackOutputAndProcess tail evmOutput acc (cursor + 1)
                
                | BytesNArraySz (_, bArr) -> 
                    let mutable offset = (cursor * 64)
                    let contents = List.init bArr.Length (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringPrepend0xBytes offset evmOutput}")
                    let acc = acc @ [BytesNArraySz (L32, contents)]
                    unpackOutputAndProcess tail evmOutput acc (cursor + bArr.Length)
                
                | BytesNArray _ ->
                    let mutable offset = (cursor * 64) |> emitSubstringAsOffset evmOutput
                    let count = (emitSubstringAsInt (cursor * 64) evmOutput)
                    offset <- offset + 64
                    let contents = List.init count (fun count ->
                        offset <- offset + (count * 64)
                        $"{emitSubstringPrepend0xBytes offset evmOutput}")
                    let acc = acc @ [BytesNArray (L32, contents)]
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

        
                    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ABI Binds
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Returns the string representation of the wrapped EVMData value, except
    /// for bool types, which are handled in `unwrapEVMBool`. EVMDatatypes that
    /// are lists return a comma-separated concatenated string.
    /// 
    let rec public unwrapEVMValue (evmDataType: EVMDatatype) =
        match evmDataType with
        | Address a -> a |> trimParameter
        | AddressArraySz a -> a |> String.concat(",") |> trimParameter
        | AddressArray a -> a |> String.concat(",") |> trimParameter
        | Uint (_,u) -> u |> trimParameter
        | UintArraySz (_, a) -> a |> String.concat(",") |> trimParameter
        | UintArray (_, a) -> a |> String.concat(",") |> trimParameter
        | Int (_, u) -> u |> trimParameter 
        | IntArraySz (_, a) -> a |> String.concat(",") |> trimParameter
        | IntArray (_, a) -> a |> String.concat(",") |> trimParameter
        | BytesN (_, b) -> b |> trimParameter
        | BytesNArraySz (_, a) -> a |> String.concat(",") |> trimParameter
        | BytesNArray (_, a) -> a |> String.concat(",") |> trimParameter
        | Bytes b -> b |> trimParameter
        | BytesArraySz b ->  b |> List.map unwrapEVMValue |> String.concat(",") |> trimParameter
        | BytesArray b -> b |> List.map unwrapEVMValue |> String.concat(",") |> trimParameter
//        | Function f -> f |> trimParameter
//        | FunctionArraySz l -> l |> String.concat(",") |> trimParameter
//        | FunctionArray l -> l |> String.concat(",") |> trimParameter
        | String s -> s |> trimParameter
        | StringArraySz b ->  b |> List.map unwrapEVMValue |> String.concat(",") |> trimParameter
        | StringArray b -> b |> List.map unwrapEVMValue |> String.concat(",") |> trimParameter
        | x -> $"{x.ToString()}"


    ///
    /// Returns the underlying boolean contained in a wrapped Bool EVMDatatype.
    /// Bools from non-array `Bool` types will return a singleton list.
    /// 
    let public unwrapEVMBool (evmBools: EVMDatatype) =
        match evmBools with
        | Bool b -> [b]
        | BoolArraySz b -> b
        | BoolArray b -> b
        | _ -> []

    
    ///
    /// Returns a function's outputs from the EVM as an EVMDatatype list. 
    let internal returnOutputAsEVMDatatypes contract functionIndicator output =
        match findFunction contract functionIndicator with
        | Ok result ->
             bindFunctionIndicator result
            |> fun s -> createOutputEvmTypes s.internalOutputs output
        | _ -> []
        
       
    ///
    /// Returns a wrapped EthAddress that has been checked for validity 
    let internal wrapEthAddress (address: string) =
        match address |> strip0x |> fun a -> a.Length = 40 with
        | true -> address |> returnChecksumAddress |> Ok
        | false -> EthAddressError |> Error
        