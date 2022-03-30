namespace web3.fs

open web3.fs.Types

module ReceiptManager =

    open RPCMethodFunctions
    open RPCParamFunctions
    open Helpers


    ///
    /// Simple polling loop for pending Transactions. No 'give up' function yet
    let rec callLoop (rpc: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>) (call: HttpRPCMessage) =
        async {
            match rpc call with
            | Error e ->
                match e with
                | RPCNullResponse ->
                    do! Async.Sleep 15000
                    return! callLoop rpc call
                | e -> return e |> Error
            | Ok o -> return o |> Ok 
        }


    ///
    /// Mailbox processor leveraging the RPCConnector to monitor the status of a transaction.
    let receiptManager (rpc: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>) (mbox: ReceiptManagerMailbox) =

        let msgLoop () =
            async {
                let! msg = mbox.Receive()
                let (ReceiptMessageAndReply (txnHash, reply)) = msg
                
                let call =
                    { method = EthMethod.GetTransactionReceipt |> wrapEthMethod
                      paramList =
                          [ txnHash.Result.Value.ToString() |> trimParameter ]
                          |> EthParam.EthParamGetTransactionReceipt
                          |> wrapEthParams
                      blockHeight = LATEST }

                let! result = callLoop rpc call
                result |> reply.Reply
            }

        msgLoop ()

    ///
    /// Returns the MailboxProcessor that oversees monitoring of Ethereum pending transactions.
    let startReceiptManager rpc =
        MailboxProcessor.Start(receiptManager rpc)


    ///
    /// Function allowing easier pipelining of RPC connection and the two-way communication channel.
    let receiptMessage (mbox: ReceiptManagerMailbox) (receipt: RPCResponse.Root) =
        mbox.PostAndReply(fun c -> ReceiptMessageAndReply(receipt, c))


    ///
    /// Returns a partially applied function ready to take an RPC connection and a previous RPC result in order to
    /// monitor an Ethereum transaction's status.
    let createReceiptMonitor rpc =
        rpc |> startReceiptManager |> receiptMessage 
