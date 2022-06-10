namespace web3.fs

open Types

[<AutoOpen>]
module ReceiptManager =

    open Common


    ///
    /// Simple polling loop for pending Transactions. No 'give up' function yet
    let rec private callLoop (rpc: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>) (call: HttpRPCMessage) =
        async {
            match rpc call with
            | Error e ->
                match e with
                | RPCNullResponse ->
                    do! Async.Sleep 7500
                    return! callLoop rpc call
                | e -> return e |> Error
            | Ok o -> return o |> Ok 
        }


    ///
    /// Mailbox processor to monitor the status of a transaction.
    let private receiptManager rpc (mbox: ReceiptManagerMailbox) =
        let rec msgLoop () =
            async {
                let! msg = mbox.Receive()
                let (ReceiptMessageAndReply (txnHash, reply)) = msg
                
                let call =
                    { method = EthMethod.GetTransactionReceipt 
                      paramList =
                          [ txnHash |> trimParameter ]
                          |> EthGenericRPC
                      blockHeight = LATEST }
                
                callLoop rpc call 
                |> Async.RunSynchronously
                |> decomposeRPCResult EthMethod.GetTransactionReceipt 
                |> reply.Reply
                
                do! msgLoop ()
                }
        
        msgLoop ()

    
    ///
    /// Returns the MailboxProcessor that monitors pending transactions.
    let private startReceiptManager rpc =
        MailboxProcessor.Start(receiptManager rpc)


    ///
    /// Function allowing easier pipelining of RPC connection.
    let private receiptMessage (mbox: ReceiptManagerMailbox) (receipt: EthTransactionHash) =
        mbox.PostAndReply(fun c -> ReceiptMessageAndReply(receipt, c))


    ///
    /// Returns a partially applied function ready to take an RPC connection and
    /// a previous RPC result in order to monitor an Ethereum transaction's
    /// status. Automatically called in 'createWeb3Environment'.
    let public createReceiptMonitor rpc =
        rpc |> startReceiptManager |> receiptMessage 
