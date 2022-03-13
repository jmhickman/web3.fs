namespace web3.fs

module RPCConnector =
    open FSharp.Data
    open FSharp.Data.HttpRequestHeaders

    open Types
    open Helpers
    open RPCMethodFunctions
    open RPCParamFunctions


    //
    // RPC helpers
    //


    let needsBlockArgs (m: RPCMethod) =
        match m with
        | EthMethod _m ->
            match _m with
            | _m when _m = EthMethod.Call || _m = EthMethod.EstimateGas -> true
            | _ -> false
        | _ -> false

    let formatRPCString (rpcMsg: HttpRPCMessage) rpcVersion blockArgs =
        match blockArgs with
        | true ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcMsg.method}","params":[{bindRPCParam rpcMsg.paramList}, "latest"], "id":1}}"""
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
                |> formatRPCString rpcMessage rpcVersion
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
    let makeEthCall (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, string>) method eParams =
        validateRPCParams eParams
        |> Result.bind (fun _params ->
            rpcConnection
                { method = method |> wrapEthMethod
                  paramList = _params |> EthParam })

    // Above to makeEthRPCCall rpc method params, explicitly for non-1559 call object transactions
    // makeEthCall rpc {from; txntype} contract functionName args
    // makeEthTxn rpc {from; txntype} contract functionName args value