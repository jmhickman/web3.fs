namespace web3.fs

module RPCConnector =
    open FSharp.Data
    open FSharp.Data.HttpRequestHeaders

    open Types
    open RPCMethodFunctions
    open RPCParamFunctions

    let needsBlockArgs (m: RPCMethod) =
        match m with
        | EthMethod _m ->
            match _m with
            | _m when _m = EthMethod.Call || _m = EthMethod.EstimateGas -> true
            | _ -> false
        | _ -> false

    let formatRPCString (rpcmsg: HttpRPCMessage) rpcVersion blockArgs =
        match blockArgs with
        | true ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcmsg.method}","params":[{bindRPCParam rpcmsg.paramlist}, "latest"], "id":1}}"""
        | false ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcmsg.method}","params":[{bindRPCParam rpcmsg.paramlist}], "id":1}}"""

    let rpcConnector url (rpcVersion: string) (mbox: HttpRPCMailbox) =
        printfn "Entered into rpcConnector..."

        let rec receiveLoop () =
            async {
                let! msg = mbox.Receive()
                let (ChannelMessageAndReply (rpcmessage, reply)) = msg

                needsBlockArgs rpcmessage.method
                |> formatRPCString rpcmessage rpcVersion
                |> fun s ->
                    printfn $"{s}"
                    s
                |> fun r ->
                    Http.RequestString(url, headers = [ ContentType HttpContentTypes.Json ], body = TextRequest r)
                |> fun s -> RPCResponse.Parse s
                |> fun s -> reply.Reply s

                do! receiveLoop ()
            }

        receiveLoop ()

    let startRpcConnector (url: string) rpcVersion =
        MailboxProcessor.Start(rpcConnector url rpcVersion)

    let channelMessage (mbox: HttpRPCMailbox) (rpcMessage: HttpRPCMessage) =
        mbox.PostAndReply(fun c -> ChannelMessageAndReply(rpcMessage, c))

    let createWeb3Connection url rpcVersion =
        (url, rpcVersion)
        ||> startRpcConnector
        |> channelMessage
