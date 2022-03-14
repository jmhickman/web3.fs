namespace web3.fs

open web3.fs.Types

module RPCConnector =
    open FSharp.Data
    open FSharp.Data.HttpRequestHeaders


    open Helpers
    open RPCMethodFunctions
    open RPCParamFunctions
    open ContractFunctions
    open ABIFunctions


    //
    // RPC helpers
    //

    let createUnvalidatedCall
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        : UnvalidatedEthParam1559Call =

        let txn, maxfee, priority, data = constantsBind constants

        let evmF =
            bindFunctionIndicator evmFunction contract

        let realArgs =
            match arguments with
            | Some a -> a
            | None -> data

        { utxnType = txn
          unonce = ""
          utoAddr = contract.address
          ufrom = constants.address
          ugas = ""
          uvalue = "" // Must be, because this is a call
          udata = $"{evmF.hash |> bindEVMSelector}{createInputByteString realArgs}"
          umaxFeePerGas = maxfee
          umaxPriorityFeePerGas = priority
          uaccessList = []
          uchainId = contract.chainId }

    ///
    /// Returns a Txn object for use in the validation function `ValidateRPCParams`
    let createUnvalidatedTxn
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        (value: string)
        : UnvalidatedEthParam1559Call =

        let txn, maxfee, priority, data = constantsBind constants

        let evmF =
            bindFunctionIndicator evmFunction contract

        let realArgs =
            match arguments with
            | Some a -> a
            | None -> data

        { utxnType = txn
          unonce = ""
          utoAddr = contract.address
          ufrom = constants.address
          ugas = ""
          uvalue = value |> bigintToHex |> padTo32BytesLeft
          udata = $"{evmF.hash |> bindEVMSelector}{createInputByteString realArgs}"
          umaxFeePerGas = maxfee
          umaxPriorityFeePerGas = priority
          uaccessList = []
          uchainId = contract.chainId }

    ///
    /// Some RPC calls require a parameter indicating at what block height the call should be performed against. This
    /// finds calls that require it in order to insert a 'latest' parameter. Will be deprecated once this argument is
    /// coming from the user.
    ///
    let needsBlockArgs (m: RPCMethod) =
        match m with
        | EthMethod _m ->
            match _m with
            | _m when _m = EthMethod.Call || _m = EthMethod.EstimateGas -> true
            | _ -> false
        | _ -> false

    let formatRPCString (rpcMsg: HttpRPCMessage) rpcVersion blockHeight blockArgs =

        match blockArgs with
        | true ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcMsg.method}","params":[{bindRPCParam rpcMsg.paramList}, "{blockHeight}"], "id":1}}"""
        | false ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcMsg.method}","params":[{bindRPCParam rpcMsg.paramList}], "id":1}}"""


    //
    // RPC connection
    //


    let rpcConnector url (rpcVersion: string) (mbox: HttpRPCMailbox) =

        let rec receiveLoop () =
            async {
                let! msg = mbox.Receive()
                let (ChannelMessageAndReply (rpcMessage, reply)) = msg

                rpcMessage.method
                |> needsBlockArgs
                |> formatRPCString rpcMessage rpcVersion rpcMessage.blockHeight
                |> fun rpcString ->
                    Http.RequestString(
                        url,
                        headers = [ ContentType HttpContentTypes.Json ],
                        body = TextRequest rpcString
                    )
                |> fun resp ->
                    let j = RPCResponse.Parse resp

                    match j.JsonValue.TryGetProperty("error") with
                    | Some e -> $"RPC error message: {e}" |> Error |> reply.Reply
                    | None -> j |> Ok |> reply.Reply

                do! receiveLoop ()
            }

        receiveLoop ()

    let startRpcConnector (url: string) rpcVersion =
        MailboxProcessor.Start(rpcConnector url rpcVersion)

    // abstracting the PostAndReply away from the caller
    let channelMessage (mbox: HttpRPCMailbox) (rpcMessage: HttpRPCMessage) =
        mbox.PostAndReply(fun c -> ChannelMessageAndReply(rpcMessage, c))

    // kick off the connection setup, returns partially applied RPC endpoint
    // Can be used in lieu of makeEthCall if the consumer wants more control
    // over how the call is handled
    let createWeb3Connection url rpcVersion =
        (url, rpcVersion)
        ||> startRpcConnector
        |> channelMessage

    // convenience function to handle common RPC case
    let makeEthCall
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, string>)
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        =

        let blockHeight' =
            match constants.blockHeight with
            | Some s -> s
            | None -> LATEST

        createUnvalidatedCall constants contract evmFunction arguments
        |> validateRPCParams
        |> Result.bind
            (fun _params ->
                rpcConnection
                    { method = EthMethod.Call |> wrapEthMethod
                      paramList = _params |> EthParam
                      blockHeight = blockHeight' })

    // Above to makeEthRPCCall rpc method params, explicitly for non-1559 call object transactions


    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain).
    /// * constants: A ContractConstants record.
    /// * contract: A deployed contract that is being called
    /// * evmFunction: The string corresponding to the name of the function being called
    let makeEthTxn
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, string>)
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        (value: string)
        =

        let blockHeight' =
            match constants.blockHeight with
            | Some s -> s
            | None -> LATEST

        createUnvalidatedTxn constants contract evmFunction arguments value
        |> validateRPCParams
        |> Result.bind
            (fun _params ->
                rpcConnection
                    { method = EthMethod.SendTransaction |> wrapEthMethod
                      paramList = _params |> EthParam
                      blockHeight = blockHeight' })
