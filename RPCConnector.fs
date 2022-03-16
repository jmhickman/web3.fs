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


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC helpers
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
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
            bindFunctionIndicator contract evmFunction

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
    /// Alias for `createUnvalidatedTxn` with the value set to 0.
    let createUnvalidatedCall
        constants
        (contract: DeployedContract)
        (evmFunction: FunctionIndicator)
        (arguments: EVMDatatype list option)
        : UnvalidatedEthParam1559Call =

        createUnvalidatedTxn constants contract evmFunction arguments "0"
        
    
    ///
    /// Some RPC calls require a parameter indicating at what block height the call should be performed against. This
    /// finds calls that require it in order to insert a parameter passed in from the user, and ignores it otherwise.
    ///
    let needsBlockArgs (m: RPCMethod) =
        match m with
        | EthMethod _m ->
            match _m with
            | _m when _m = EthMethod.Call || _m = EthMethod.EstimateGas -> true
            | _ -> false
        | _ -> false

    
    ///
    /// Returns properly formatted JSON-RPC values to send to the RPC.
    let formatRPCString (rpcMsg: HttpRPCMessage) rpcVersion blockHeight blockArgs =

        match blockArgs with
        | true ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcMsg.method}","params":[{bindRPCParam rpcMsg.paramList}, "{blockHeight}"], "id":1}}"""
        | false ->
            $"""{{"jsonrpc":"{rpcVersion}","method":"{bindRPCMethod rpcMsg.method}","params":[{bindRPCParam rpcMsg.paramList}], "id":1}}"""


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC connection functions
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


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
                    | Some e -> $"RPC error message: {e}" |> ConnectionError |> Error |> reply.Reply
                    | None -> j |> Ok |> reply.Reply

                do! receiveLoop ()
            }

        receiveLoop ()

    
    ///
    /// Returns the MailboxProcessor that oversees Http communications with the RPC
    let startRpcConnector (url: string) rpcVersion =
        MailboxProcessor.Start(rpcConnector url rpcVersion)

    
    ///
    /// Function allowing easier pipelining of RPC connection and the two-way communication channel. 
    let channelMessage (mbox: HttpRPCMailbox) (rpcMessage: HttpRPCMessage) =
        mbox.PostAndReply(fun c -> ChannelMessageAndReply(rpcMessage, c))

    
    ///
    /// Returns a partially applied function ready to take an HttpRPCMessage and send it to the RPC endpoint and
    /// return a Result.
    /// 
    let createWeb3Connection url rpcVersion =
        (url, rpcVersion)
        ||> startRpcConnector
        |> channelMessage

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Ethereum call functions using the RPC connection
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    

    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain).
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionIndicator corresponding to the the function being called. May be a string (cast to a
    /// ByString) or a IndicatedFunction (returned by `findFunction`)
    /// * arguments: a list option of EVMDatatypes. May use `None` or `(Some [])` to indicate no arguments.
    /// value: the wei-denominated amount of ETH to send along with a transaction to a `payable` function.
    /// 
    let makeEthTxn
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>)
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

    
    ///
    /// Creates an Ethereum call that does NOT change the state of the blockchain.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A DeployedContract that is being called
    /// * evmFunction: FunctionIndicator corresponding to the the function being called. May be a string (cast to a
    /// ByString) or a IndicatedFunction (returned by `findFunction`)
    /// * arguments: a list option of EVMDatatypes. May use `None` or `(Some [])` to indicate no arguments.
    /// 
    let makeEthCall
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>)
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
            
    
    ///
    /// Creates an Ethereum transaction (a call that changes the state of the blockchain) specifically for deploying
    /// a contract's bytecode.
    /// * rpcConnection: An activated RPC connection from `createWeb3Connection`
    /// * constants: A ContractConstants record.
    /// * contract: A UndeployedContract that is being deployed
    ///  
    let deployEthContract
        (rpcConnection: HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>)
        (constants: ContractConstants)
        (contract: UndeployedContract)
        =
        
        let (RawContractBytecode _rawBytecode) = contract.bytecode
        let txn, maxfee, priority, _ = constantsBind constants

        let cArgs =
            match contract.constructorArguments with
            | Some a -> a
            | None -> []                   
       
        { utxnType = txn
          unonce = ""
          utoAddr = ""
          ufrom = constants.address
          ugas = ""
          uvalue = "0" |> bigintToHex |> padTo32BytesLeft
          udata = $"{_rawBytecode}{contract.constructor |> bindEVMSelector}{createInputByteString cArgs}" |> prepend0x
          umaxFeePerGas = maxfee
          umaxPriorityFeePerGas = priority
          uaccessList = []
          uchainId = contract.chainId }
        
        |> validateRPCParams
        |> Result.bind
            (fun _params ->
                rpcConnection
                    { method = EthMethod.SendTransaction |> wrapEthMethod
                      paramList = _params |> EthParam
                      blockHeight = LATEST })
