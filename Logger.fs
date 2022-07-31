namespace Web3.fs

module Logger =
    
    open System
    
    ///
    /// Color bindings.
    let internal setColor color = 
        match color with
        | Blue -> Console.ForegroundColor <- ConsoleColor.Blue
        | DarkBlue -> Console.ForegroundColor <- ConsoleColor.DarkBlue
        | Yellow -> Console.ForegroundColor <- ConsoleColor.Yellow
        | Green -> Console.ForegroundColor <- ConsoleColor.Green
        | Red -> Console.ForegroundColor <- ConsoleColor.Red
        | Gold -> Console.ForegroundColor <- ConsoleColor.DarkYellow
        
    
    ///
    /// Revert the color.
    let internal revertColor () = Console.ResetColor()
    
    
    ///
    /// Prints the message, reverts the color.
    let internal printAndRevert (color: WConsoleColor) message =
        setColor color
        printf $"{message}"
        revertColor ()
    
    
    ///
    /// Formatted print of a simple response value, typically an RPC response
    let private prettySimple (simple: string) =
        printAndRevert Blue $"{simple |> trimParameter}\n"
        
    
    ///
    /// Formatted print of a TransactionReceipt
    let private prettyTransactionReceipt (receipt: TransactionReceipt) =
        let toOrContract =
            if receipt.contractAddress.IsSome then
                $"Contract deployed to: {receipt.contractAddress.Value}\n"
            else $"To: {receipt.toAddr.Value}\n"
        
        let logs =
            if receipt.logs.Length > 0 then
                receipt.logs
                |> List.toArray
                |> Array.map(fun s -> "\t" + s)
                |> fun a -> String.Join("\n", a)
            else ""
        
        if receipt.status = "0x0" then
            printAndRevert Red "Failed transaction\n"
        else
            printAndRevert Green "Successful Transaction\n"
        printAndRevert Blue $"Transaction hash: {receipt.transactionHash}\n"
        printAndRevert DarkBlue $"From: {receipt.from}\n"
        printAndRevert Blue toOrContract
        printAndRevert Gold $"Gas used: {receipt.gasUsed |> hexToBigintUnsigned}\n"
        printAndRevert Gold $"Effective gas price: {receipt.effectiveGasPrice |> hexToBigintUnsigned}\n"
        printAndRevert Gold $"Cumulative gas used: {receipt.cumulativeGasUsed |> hexToBigintUnsigned}\n"
        printfn $"Block hash: {receipt.blockHash}"
        printfn $"Block number: {receipt.blockNumber}"
        printfn $"Index of transaction in block: {receipt.transactionIndex}"
        printfn $"Type: {receipt.tType}"
        printAndRevert Blue "Logs:\n"
        printfn $"{logs}"
        
        
    ///
    /// Formatted print of Transaction records
    let private prettyTransaction (txn: MinedTransaction) =
        let accessList =
            if txn.accessList.Length > 0 then
                txn.accessList
                |> List.toArray
                |> Array.map(fun s -> "\t" + s)
                |> fun a -> String.Join("\n", a)
            else ""

        let toAddress = txn.toAddr |> Option.defaultValue "Deployment transaction"
        
        printAndRevert Blue $"Transaction hash: {txn.hash}\n"
        printAndRevert DarkBlue $"From: {txn.from}\n"
        printAndRevert Blue $"To: {toAddress}\n"
        printAndRevert Blue "Nonce: "
        printfn $"{txn.nonce}"
        printAndRevert Blue "Input: "
        printfn $"{txn.input}"
        printAndRevert Gold $"Value: {txn.value |> hexToBigintUnsigned}\n"
        printAndRevert Gold "Gas: "
        printfn $"{txn.gas |> hexToBigintUnsigned}"
        printAndRevert Gold "Gas price: "
        printfn $"{txn.gasPrice |> hexToBigintUnsigned}"
        printAndRevert Gold "Max fee per gas: "
        printfn $"{txn.maxFeePerGas |> hexToBigintUnsigned}"
        printAndRevert Gold "Max priority fee per gas: "
        printfn $"{txn.maxPriorityFeePerGas |> hexToBigintUnsigned}"
        printfn "Access list: "
        printfn $"{accessList}"
        printfn $"Block hash: {txn.blockHash}"
        printfn $"Block number: {txn.blockNumber |> hexToBigintUnsigned}"
        printfn $"Chain ID: {txn.chainId}"
        printfn $"R: {txn.r}"
        printfn $"S: {txn.s}"
        printfn $"V: {txn.v}"
        printfn $"Index in block: {txn.transactionIndex}"
        printfn $"Transaction type: {txn.tType}"    
    
        
    ///
    /// Formatted print of a Block record
    let private prettyBlock (block: EthBlock) =
        let sealFields =
            if block.sealFields.Length > 0 then
                block.sealFields
                |> List.toArray
                |> Array.map(fun s -> "\t" + s)
                |> fun a -> String.Join("\n", a)
            else ""
        
        let uncles =
            if block.uncles.Length > 0 then
                block.uncles
                |> List.toArray
                |> Array.map(fun s -> "\t" + s)
                |> fun a -> String.Join("\n", a)
            else ""
        
        let transactions =
            if block.transactions.Length > 0 then
                block.transactions
                |> List.toArray
                |> Array.map(fun s -> "\t" + s)
                |> fun a -> String.Join("\n", a)
            else ""
        
        
        printAndRevert Blue $"Ethereum Block: {block.number}\n"
        printfn $"Block author: {block.author}"
        printAndRevert Blue "Miner: "
        printfn $"{block.miner}"
        printAndRevert Gold "Base fee per gas: "
        printfn $"{block.baseFeePerGas |> hexToBigintUnsigned}"
        printAndRevert Gold "Gas limit: "
        printfn $"{block.gasLimit |> hexToBigintUnsigned}"
        printAndRevert Gold "Gas used: "
        printfn $"{block.gasUsed |> hexToBigintUnsigned}"
        printfn $"Difficulty: {block.difficulty}"
        printfn $"Extra data: {block.extraData}"
        printfn $"Parent hash: {block.parentHash}"
        printfn $"Receipts root: {block.receiptsRoot}"
        printfn $"Seal fields: {sealFields}"
        printfn $"Uncles hash: {block.sha3Uncles}"
        printAndRevert Blue "Block size: "
        printfn $"{block.size}"
        printf $"Stateroot: {block.stateRoot}\n"
        printAndRevert Blue "Timestamp: "
        printfn $"{block.timestamp}"
        printfn $"Total difficulty: {block.totalDifficulty}"
        printfn $"Transactions root: {block.transactionsRoot}"
        printfn $"Uncles: {uncles}"
        printAndRevert Blue "Transactions:\n"
        printfn $"{transactions}"
        
    
    ///
    /// Formatted print of a call result 
    let rec private prettyCallResult (callResult: EVMDatatype list) =
        match callResult with
        | head::tail ->
            match head with
            | Uint (bitness, s) ->
                let _bitness = bitness.ToString().Replace("B", EMPTY)
                let _s = s.Replace(QUOTE, EMPTY)
                printAndRevert Blue "Call response: "
                printfn $"Uint{_bitness} {_s}"
                prettyCallResult tail
            | Int (bitness, s) ->
                let _bitness = bitness.ToString().Replace("B", EMPTY)
                let _s = s.Replace(QUOTE, EMPTY)
                printAndRevert Blue "Call response: "
                printfn $"Int{_bitness} {_s}"
                prettyCallResult tail
            | BytesN (byteLength, s) ->
                let _len = byteLength.ToString().Replace("L", EMPTY)
                let _s = s.Replace(QUOTE, EMPTY)
                printAndRevert Blue "Call response: "
                printfn $"Byte{_len} {_s}"
            | s ->
                let _s = s.ToString().Replace(QUOTE, EMPTY)
                printAndRevert Blue "Call response: "
                printfn $"{_s}"
                prettyCallResult tail
        | [] -> ()

        
    ///
    /// Emits console messages with color and glyphs based on the incoming
    /// message. I'm not certain these locks are required.
    /// 
    let private loggerMailbox (mbox: Logger) =
        let locker = Object
        let rec receiver () =
            async {
                let! msg = mbox.Receive()
                let logType, message = msg
                match logType with
                | Warn ->
                    lock locker (fun _ ->
                    printAndRevert Yellow "[-]  "
                    printfn $"{message}")
                | Failure ->
                    lock locker (fun _ ->
                    printAndRevert Red "[!]  "
                    printfn $"{message}")
                | Success ->
                    lock locker (fun _ ->
                    match message with
                    | Library s ->
                        printAndRevert DarkBlue "[@] "
                        prettySimple s
                    | _ ->                     
                        printAndRevert Green "[+]  Success\n"
                        match message with
                        | Block ethBlock -> prettyBlock ethBlock
                        | Transaction mTransaction -> prettyTransaction mTransaction
                        | TransactionReceiptResult receipt -> prettyTransactionReceipt receipt
                        | CallResult evmDatatypes -> prettyCallResult evmDatatypes
                        | SimpleValue s -> prettySimple s
                        | _ -> () )
                
                do! receiver () 
            }
        receiver ()
        
    
    let internal startLogger () = MailboxProcessor.Start(loggerMailbox)