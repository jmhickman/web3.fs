namespace web3.fs

module Types =

    open FSharp.Data
    open FSharp.Json


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Misc types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let weiDiv = 1000000000000000000I

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Type Provider parser setup
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    // Experiment with making this one sample as inclusive as possible and see if it handles the minimal case(s) properly. Better to have just one.
    [<Literal>]
    let nullable =
        """[{"id":1,"jsonrpc":"2.0","result":{"blockHash":"0xc3646d4d8e3c650b15ef7f8a4d6d16fa4c7e68eb08195361182d7aa2eb3a0d65","blockNumber":"0x9dc3fe","contractAddress":"0x3872353821064f55df53ad1e2d7255e969f6eac0","cumulativeGasUsed":"0x1166efb","effectiveGasPrice":"0x650fe5cd5","from":"0x2268b96e204379ee8366505c344ebe5cc34d3a46","gasUsed":"0x2e707","logs":[],"logsBloom":"0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000","status":"0x1","to":null,"transactionHash":"0x0bd6acf13c1adf63c1f2b17ac1f9c4b98d94f2701728ffd5efe50aa77a6aa5aa","transactionIndex":"0x56","type":"0x2"}}, {"id":1,"jsonrpc":"2.0","result":null}]"""

    type NullableRPCResponse = JsonProvider<nullable, SampleIsList=true>

    [<Literal>]
    let success =
        """{"id":1,"jsonrpc":"2.0","result":{"blockHash":"0xc3646d4d8e3c650b15ef7f8a4d6d16fa4c7e68eb08195361182d7aa2eb3a0d65","blockNumber":"0x9dc3fe","contractAddress":"0x3872353821064f55df53ad1e2d7255e969f6eac0","cumulativeGasUsed":"0x1166efb","effectiveGasPrice":"0x650fe5cd5","from":"0x2268b96e204379ee8366505c344ebe5cc34d3a46","gasUsed":"0x2e707","logs":[],"logsBloom":"0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000","status":"0x1","to":null,"transactionHash":"0x0bd6acf13c1adf63c1f2b17ac1f9c4b98d94f2701728ffd5efe50aa77a6aa5aa","transactionIndex":"0x56","type":"0x2"}}"""

    type RPCResponse = JsonProvider<success>
    
    [<Literal>]
    let sampleABI =
        """[{"type":"error","inputs": [{"name":"available","type":"uint256"},{"name":"required","type":"uint256"}],"name":"InsufficientBalance"}, {"type":"event","inputs": [{"name":"a","type":"uint256","indexed":true},{"name":"b","type":"bytes32","indexed":false}],"name":"Event"}, {"type":"event","inputs": [{"name":"a","type":"uint256","indexed":true},{"name":"b","type":"bytes32","indexed":false}],"name":"Event2"}, {"type":"function","inputs": [{"name":"a","type":"uint256"}],"name":"foo","outputs": []}]"""

    type ParsedABI = JsonProvider<sampleABI, SampleIsList=true>

    
    [<Literal>]
    let sampleBytecode =
        """{"functionDebugData":{},"generatedSources":[],"linkReferences":{},"object":"608060405234801561001057600080fd5b5061027a806100206000396000f3fe608060405234801561001057600080fd5b506004361061002b5760003560e01c806357172feb14610030575b600080fd5b61004a600480360381019061004591906100db565b610060565b6040516100579190610133565b60405180910390f35b600060019050919050565b600061007e61007984610173565b61014e565b90508281526020810184848401111561009a57610099610224565b5b6100a58482856101b0565b509392505050565b600082601f8301126100c2576100c161021f565b5b81356100d284826020860161006b565b91505092915050565b6000602082840312156100f1576100f061022e565b5b600082013567ffffffffffffffff81111561010f5761010e610229565b5b61011b848285016100ad565b91505092915050565b61012d816101a4565b82525050565b60006020820190506101486000830184610124565b92915050565b6000610158610169565b905061016482826101bf565b919050565b6000604051905090565b600067ffffffffffffffff82111561018e5761018d6101f0565b5b61019782610233565b9050602081019050919050565b60008115159050919050565b82818337600083830152505050565b6101c882610233565b810181811067ffffffffffffffff821117156101e7576101e66101f0565b5b80604052505050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052604160045260246000fd5b600080fd5b600080fd5b600080fd5b600080fd5b6000601f19601f830116905091905056fea264697066735822122019ba83a5b2836d1ab300440fc2bcaaa96772566d18c40aaa128366bf6a3ed13a64736f6c63430008070033","opcodes":"PUSH10x80PUSH10x40MSTORECALLVALUEDUP1ISZEROPUSH20x10JUMPIPUSH10x0DUP1REVERTJUMPDESTPOPPUSH20x27ADUP1PUSH20x20PUSH10x0CODECOPYPUSH10x0RETURNINVALIDPUSH10x80PUSH10x40MSTORECALLVALUEDUP1ISZEROPUSH20x10JUMPIPUSH10x0DUP1REVERTJUMPDESTPOPPUSH10x4CALLDATASIZELTPUSH20x2BJUMPIPUSH10x0CALLDATALOADPUSH10xE0SHRDUP1PUSH40x57172FEBEQPUSH20x30JUMPIJUMPDESTPUSH10x0DUP1REVERTJUMPDESTPUSH20x4APUSH10x4DUP1CALLDATASIZESUBDUP2ADDSWAP1PUSH20x45SWAP2SWAP1PUSH20xDBJUMPJUMPDESTPUSH20x60JUMPJUMPDESTPUSH10x40MLOADPUSH20x57SWAP2SWAP1PUSH20x133JUMPJUMPDESTPUSH10x40MLOADDUP1SWAP2SUBSWAP1RETURNJUMPDESTPUSH10x0PUSH10x1SWAP1POPSWAP2SWAP1POPJUMPJUMPDESTPUSH10x0PUSH20x7EPUSH20x79DUP5PUSH20x173JUMPJUMPDESTPUSH20x14EJUMPJUMPDESTSWAP1POPDUP3DUP2MSTOREPUSH10x20DUP2ADDDUP5DUP5DUP5ADDGTISZEROPUSH20x9AJUMPIPUSH20x99PUSH20x224JUMPJUMPDESTJUMPDESTPUSH20xA5DUP5DUP3DUP6PUSH20x1B0JUMPJUMPDESTPOPSWAP4SWAP3POPPOPPOPJUMPJUMPDESTPUSH10x0DUP3PUSH10x1FDUP4ADDSLTPUSH20xC2JUMPIPUSH20xC1PUSH20x21FJUMPJUMPDESTJUMPDESTDUP2CALLDATALOADPUSH20xD2DUP5DUP3PUSH10x20DUP7ADDPUSH20x6BJUMPJUMPDESTSWAP2POPPOPSWAP3SWAP2POPPOPJUMPJUMPDESTPUSH10x0PUSH10x20DUP3DUP5SUBSLTISZEROPUSH20xF1JUMPIPUSH20xF0PUSH20x22EJUMPJUMPDESTJUMPDESTPUSH10x0DUP3ADDCALLDATALOADPUSH80xFFFFFFFFFFFFFFFFDUP2GTISZEROPUSH20x10FJUMPIPUSH20x10EPUSH20x229JUMPJUMPDESTJUMPDESTPUSH20x11BDUP5DUP3DUP6ADDPUSH20xADJUMPJUMPDESTSWAP2POPPOPSWAP3SWAP2POPPOPJUMPJUMPDESTPUSH20x12DDUP2PUSH20x1A4JUMPJUMPDESTDUP3MSTOREPOPPOPJUMPJUMPDESTPUSH10x0PUSH10x20DUP3ADDSWAP1POPPUSH20x148PUSH10x0DUP4ADDDUP5PUSH20x124JUMPJUMPDESTSWAP3SWAP2POPPOPJUMPJUMPDESTPUSH10x0PUSH20x158PUSH20x169JUMPJUMPDESTSWAP1POPPUSH20x164DUP3DUP3PUSH20x1BFJUMPJUMPDESTSWAP2SWAP1POPJUMPJUMPDESTPUSH10x0PUSH10x40MLOADSWAP1POPSWAP1JUMPJUMPDESTPUSH10x0PUSH80xFFFFFFFFFFFFFFFFDUP3GTISZEROPUSH20x18EJUMPIPUSH20x18DPUSH20x1F0JUMPJUMPDESTJUMPDESTPUSH20x197DUP3PUSH20x233JUMPJUMPDESTSWAP1POPPUSH10x20DUP2ADDSWAP1POPSWAP2SWAP1POPJUMPJUMPDESTPUSH10x0DUP2ISZEROISZEROSWAP1POPSWAP2SWAP1POPJUMPJUMPDESTDUP3DUP2DUP4CALLDATACOPYPUSH10x0DUP4DUP4ADDMSTOREPOPPOPPOPJUMPJUMPDESTPUSH20x1C8DUP3PUSH20x233JUMPJUMPDESTDUP2ADDDUP2DUP2LTPUSH80xFFFFFFFFFFFFFFFFDUP3GTORISZEROPUSH20x1E7JUMPIPUSH20x1E6PUSH20x1F0JUMPJUMPDESTJUMPDESTDUP1PUSH10x40MSTOREPOPPOPPOPJUMPJUMPDESTPUSH320x4E487B7100000000000000000000000000000000000000000000000000000000PUSH10x0MSTOREPUSH10x41PUSH10x4MSTOREPUSH10x24PUSH10x0REVERTJUMPDESTPUSH10x0DUP1REVERTJUMPDESTPUSH10x0DUP1REVERTJUMPDESTPUSH10x0DUP1REVERTJUMPDESTPUSH10x0DUP1REVERTJUMPDESTPUSH10x0PUSH10x1FNOTPUSH10x1FDUP4ADDANDSWAP1POPSWAP2SWAP1POPJUMPINVALIDLOG2PUSH50x69706673580x22SLTKECCAK256NOT0xBADUP40xA50xB2DUP4PUSH140x1AB300440FC2BCAAA96772566D180xC4EXP0xAASLTDUP4PUSH70xBF6A3ED13A6473PUSH160x6C634300080700330000000000000000","sourceMap":"142:117:0:-:0;;;;;;;;;;;;;;;;;;;"}"""
   
    type ContractBytecode = JsonProvider<sampleBytecode>
    
    let rpcNullBind = """{"id":1,"jsonrpc":"2.0","result":"null"}""" 
    
    
    // found how to fix this, will address later
    //type RPCBlockResponse = JsonProvider<"./Samples/blocksample.json">
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Transaction Envelope types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    let EARLIEST = "earliest"
    let LATEST = "latest"
    let PENDING = "pending"
    let ZERO = "0x0"
    let fakedOffset = "0000000000000000000000000000000000000000000000000000000000000020"
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // EVM data types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /// Representations of EVM types in simplistic form. There is no effort made to check that provided values of these
    /// types conform to any limitation of said types.
    ///
    /// **Warning** Note that while the Solidity documentation assert various `fixed` and `ufixed` types (describing
    /// very large floats with ranges of precision for the whole and fractional parts), they are currently only 
    /// partially implemented in the EVM and so are unsupported here at this time.
    ///
    /// **Warning** There is no attempt to create types for multidimensional arrays. In practice their use is very rare,
    /// but these types don't support 'composition' into such arrays.
    ///
    type EVMDatatype =
        | Tuple of EVMDatatype list //
        | TupleArray of EVMDatatype list //
        | TupleArraySz of EVMDatatype list // 
        | Address of string //
        | AddressArraySz of string list //
        | AddressArray of string list //
        | Uint256 of string //
        | Uint256ArraySz of string list //
        | Uint256Array of string list //
        | Int256 of string //
        | Int256ArraySz of string list // 
        | Int256Array of string list //
        | Bool of bool //
        | BoolArraySz of bool list //
        | BoolArray of bool list //
        | BytesSz of string //
        | BytesSzArraySz of string list // 
        | BytesSzArray of string list // 
        | Bytes of string //
        | BytesArraySz of EVMDatatype list //
        | BytesArray of EVMDatatype list //
        | Function of string
        | FunctionArray of string list
        | FunctionArraySz of string list
        | String of string //
        | StringArraySz of EVMDatatype list //
        | StringArray of EVMDatatype list //
        | Blob of string

    
    type EVMTypeSignaling =
        | EVMUint8
        | EVMUint16
        | EVMUint32
        | EVMUint64
        | EVMUint128
        | EVMInt8
        | EVMInt16
        | EVMInt32
        | EVMInt64
        | EVMInt128
        
    

    // just aliases, may adapt validators later to emit stronger types
    type TxnType = string
    type Quantity = string
    type Data = string
    type EthAddress = string
    type EthTransactionHash = string
    type AccessList = (EthAddress * Data list) list

    
    ///
    /// A 'raw' representation of an EIP-1559 compliant txn object. This is fed through a validation function
    /// only to ensure that the bytes meet certain criteria (documented in Helpers.fs)
    type UnvalidatedEthParam1559Call =
        { utxnType: string
          unonce: string
          utoAddr: string
          ufrom: string
          ugas: string
          uvalue: string
          udata: string
          umaxPriorityFeePerGas: string
          umaxFeePerGas: string
          uaccessList: AccessList
          uchainId: string }
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Parameter Types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    type EthParamAccounts = string list
    type EthParamBlockNumber = string list


    // There is a tremendous amount of leeway in acceptable RPC messages in terms of what is
    // included. This makes the type pretty gross-looking. `data` is the only properly 'required'
    // value, and in practice toAddr and from will be present as well (but not always!)
    type EthParam1559Call =
        { [<JsonField("type")>]
          txnType: TxnType option // Seems optional, needs research.
          nonce: Quantity option // Missing nonce is fine, if talking to a wallet
          [<JsonField("to")>]
          toAddr: EthAddress option // Missing to is fine, if deploying a contract
          from: EthAddress option // Missing 'from' is not great, may cause errors if wallet or node can't determine sender
          gas: Quantity option // Missing gas limit is fine, if talking to wallet
          value: Quantity option // Missing value is fine, if making a Call. May still be fine if txn isn't payable
          data: Data // Calls, txn and deploys should always have a valid data value
          maxPriorityFeePerGas: Quantity option // Missing gas is fine, if talking to wallet
          maxFeePerGas: Quantity option // Missing gas is fine, if talking to wallet
          accessList: AccessList option // Typically empty
          chainId: Quantity option } // Seems to be genuinely optional at this time, but should be included.

    type EthParamCoinbase = string list
    type EthParamCompileLLL = string list
    type EthParamCompileSerpent = string list
    type EthParamCompileSolidity = string list
    type EthParam1559EstimateGas = EthParam1559Call
    type EthParamGasPrice = string list
    type EthParamGetBalance = string list
    type EthParamGetBlockByHash = string list
    type EthParamGetBlockByNumber = string list
    type EthParamGetBlockTransactionCountByHash = string list
    type EthParamGetBlockTransactionCountByNumber = string list
    type EthParamGetCode = string list
    type EthParamGetCompilers = string list
    type EthParamGetFilterChanges = string list
    type EthParamGetFilterLogs = string list

    // provisional
    type EthParamGetLogs =
        { fromBlock: string option
          toBlock: string option
          address: string option
          topics: string array option
          blockhash: string option }

    type EthParamGetStorageAt = string list
    type EthParamGetTransactionCount = string list
    type EthParamGetTransactionByHash = string list
    type EthParamGetTransactionByBlockHashAndIndex = string list
    type EthParamGetTransactionByBlockNumberAndIndex = string list
    type EthParamGetTransactionReceipt = string list
    type EthParamGetUncleByBlockHashAndIndex = string list
    type EthParamGetUncleByBlockNumberAndIndex = string list
    type EthParamGetUncleCountByBlockHash = string list
    type EthParamGetUncleCountByBlockNumber = string list
    type EthParamGetWork = string list
    type EthParamHashrate = string list
    type EthParamMining = string list

    // provisional
    type EthParamNewFilter =
        { fromBlock: string option
          toBlock: string option
          address: string option
          topics: string array option }

    type EthParamNewBlockFilter = string list
    type EthParamNewPendingTransactionFilter = string list
    type EthParamProtocolVersion = string list
    type EthParamSyncing = string list
    type EthParam1559SendTransaction = EthParam1559Call
    type EthParamSendRawTransaction = string list
    type EthParamSign = string list
    type EthParam1559SignTransaction = EthParam1559Call
    type EthParamSubmitWork = string list
    type EthParamSubmitHashRate = string list
    type EthParamUninstallFilter = string list

    type EthParam =
        | EthParamAccounts of EthParamAccounts
        | EthParamBlockNumber of EthParamBlockNumber
        | EthParam1559Call of EthParam1559Call
        | EthParamCoinbase of EthParamCoinbase
        | EthParamCompileLLL of EthParamCompileLLL
        | EthParamCompileSerpent of EthParamCompileSerpent
        | EthParamCompileSolidity of EthParamCompileSolidity
        | EthParam1559EstimateGas of EthParam1559EstimateGas
        | EthParamGasPrice of EthParamGasPrice
        | EthParamGetBalance of EthParamGetBalance
        | EthParamGetBlockByHash of EthParamGetBlockByHash
        | EthParamGetBlockByNumber of EthParamGetBlockByNumber
        | EthParamGetBlockTransactionCountByHash of EthParamGetBlockTransactionCountByHash
        | EthParamGetBlockTransactionCountByNumber of EthParamGetBlockTransactionCountByNumber
        | EthParamGetCode of EthParamGetCode
        | EthParamGetCompilers of EthParamGetCompilers
        | EthParamGetFilterChanges of EthParamGetFilterChanges
        | EthParamGetFilterLogs of EthParamGetFilterLogs
        | EthParamGetLogs of EthParamGetLogs
        | EthParamGetStorageAt of EthParamGetStorageAt
        | EthParamGetTransactionCount of EthParamGetTransactionCount
        | EthParamGetTransactionByHash of EthParamGetTransactionByHash
        | EthParamGetTransactionByBlockHashAndIndex of EthParamGetTransactionByBlockHashAndIndex
        | EthParamGetTransactionByBlockNumberAndIndex of EthParamGetTransactionByBlockNumberAndIndex
        | EthParamGetTransactionReceipt of EthParamGetTransactionReceipt
        | EthParamGetUncleByBlockHashAndIndex of EthParamGetUncleByBlockHashAndIndex
        | EthParamGetUncleByBlockNumberAndIndex of EthParamGetUncleByBlockNumberAndIndex
        | EthParamGetUncleCountByBlockHash of EthParamGetUncleCountByBlockHash
        | EthParamGetUncleCountByBlockNumber of EthParamGetUncleCountByBlockNumber
        | EthParamGetWork of EthParamGetWork
        | EthParamHashrate of EthParamHashrate
        | EthParamMining of EthParamMining
        | EthParamNewFilter of EthParamNewFilter
        | EthParamNewBlockFilter of EthParamNewBlockFilter
        | EthParamNewPendingTransactionFilter of EthParamNewPendingTransactionFilter
        | EthParamProtocolVersion of EthParamProtocolVersion
        | EthParamSyncing of EthParamSyncing
        | EthParam1559SendTransaction of EthParam1559SendTransaction
        | EthParamSendRawTransaction of EthParamSendRawTransaction
        | EthParamSign of EthParamSign
        | EthParam1559SignTransaction of EthParam1559SignTransaction
        | EthParamSubmitWork of EthParamSubmitWork
        | EthParamSubmitHashRate of EthParamSubmitHashRate
        | EthParamUninstallFilter of EthParamUninstallFilter

    type NetListening = string list
    type NetPeerCount = string list
    type NetVersion = string list

    type NetParam =
        | NetListening of NetListening
        | NetPeerCount of NetPeerCount
        | NetVersion of NetVersion

    type Web3ClientVersion = string list
    type Web3Sha3 = string list

    type Web3Param =
        | Web3ClientVersion of Web3ClientVersion
        | Web3Sha3 of Web3Sha3

    type RPCParams =
        | EthParam of EthParam
        | NetParam of NetParam
        | Web3Param of Web3Param


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Method Types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    [<RequireQualifiedAccess>]
    type EthMethod =
        | Accounts
        | BlockNumber
        | Call
        | Coinbase
        | CompileLLL
        | CompileSerpent
        | CompileSolidity
        | EstimateGas
        | GasPrice
        | GetBalance
        | GetBlockByHash
        | GetBlockByNumber
        | GetBlockTransactionCountByHash
        | GetBlockTransactionCountByNumber
        | GetCode
        | GetCompilers
        | GetFilterChanges
        | GetFilterLogs
        | GetLogs
        | GetStorageAt
        | GetTransactionCount
        | GetTransactionByHash
        | GetTransactionByBlockHashAndIndex
        | GetTransactionByBlockNumberAndIndex
        | GetTransactionReceipt
        | GetUncleByBlockHashAndIndex
        | GetUncleByBlockNumberAndIndex
        | GetUncleCountByBlockHash
        | GetUncleCountByBlockNumber
        | GetWork
        | Hashrate
        | Mining
        | NewFilter
        | NewBlockFilter
        | NewPendingTransactionFilter
        | ProtocolVersion
        | Syncing
        | SendTransaction
        | SendRawTransaction
        | Sign
        | SignTransaction
        | SubmitWork
        | SubmitHashRate
        | UninstallFilter

    [<RequireQualifiedAccess>]
    type NetMethod =
        | Version
        | PeerCount
        | Listening

    [<RequireQualifiedAccess>]
    type Web3Method =
        | ClientVersion
        | Sha3

    [<RequireQualifiedAccess>]
    type ShhMethod =
        | AddToGroup
        | GetFilterChanges
        | GetMessages
        | HasIdentity
        | NewIdentity
        | NewFilter
        | NewGroup
        | Post
        | UninstallFilter
        | Version

    type RPCMethod =
        | EthMethod of EthMethod
        | NetMethod of NetMethod
        | ShhMethod of ShhMethod
        | Web3Method of Web3Method


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Message type
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    type HttpRPCMessage =
        { method: RPCMethod
          paramList: RPCParams
          blockHeight: string }

    
    ///
    /// Overall type for errors in various places in the pipeline. Probably not final at all.
    type Web3Error =
        | ContractParseFailure of string
        | ConnectionError of string
        | DataValidatorError of string
        | HttpClientError of string
        | RPCResponseErrorOrNull of string
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // MailboxProcessor types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    type MailboxTransaction =
        TransactionMessageAndReply of HttpRPCMessage * AsyncReplyChannel<Result<RPCResponse.Root, Web3Error>>
    type HttpRPCMailbox = MailboxProcessor<MailboxTransaction>

    type MailboxReceiptManager =
        ReceiptMessageAndReply of RPCResponse.Root * AsyncReplyChannel<Result<RPCResponse.Root, Web3Error>>
    type ReceiptManagerMailbox = MailboxProcessor<MailboxReceiptManager>

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Contract types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Set of tupled values created during Json parsing and filtering
    type IntermediateFunctionRepresentation =
        option<JsonValue> * option<JsonValue> * option<JsonValue> * option<JsonValue>


    ///
    /// Set of tupled values created during Json parsing and filtering
    type IntermediateEventRepresentation = option<JsonValue> * option<JsonValue> * option<JsonValue>


    ///
    /// Set of tupled values created during Json parsing and filtering
    type IntermediateErrorRepresentation = option<JsonValue> * option<JsonValue>


    ///
    /// Result of the 'flattening' of the JSON representation of an EVM function. Used as the input to
    /// the Keccak (SHA3) hash generated to provide the 'function selector' of a function and
    /// only relevant for that purpose.
    ///
    type CanonicalRepresentation =
        | CanonicalFunctionRepresentation of string
        | CanonicalEventRepresentation of string
        | CanonicalErrorRepresentation of string


    ///
    /// Output of the process of extracting EVM function strings from Json.
    type EVMFunctionInputs = EVMFunctionInputs of string


    ///
    /// Output of the process of extracting EVM function strings from Json.
    type EVMFunctionOutputs = EVMFunctionOutputs of string


    ///
    /// The first 4 bytes of the Keccak256 hash of the function's canonical representation.
    type EVMSelector =
        | EVMFunctionHash of string
        | EVMEventSelector of string


    ///
    /// Describes the mutability of the function.
    /// * Pure: No reads from blockchain state, no writes to blockchain state.
    /// * View: Reads state, no writes to blockchain state.
    /// * Nonpayable: Doesn't require an amount of ETH in the 'value' parameter of the txn object. Changes chain state.
    /// * Payable: Accepts a value from the 'value' parameter of the txn object. Changes chain state.
    ///
    type StateMutability =
        | Pure
        | View
        | Nonpayable
        | Payable


    ///
    /// Represents a single function exposed by a Solidity contract.
    /// * name: Name of the function from the source code
    /// * hash: The 'function selector' hash, the Keccak256 hash of the 'canonical representation' of the function.
    /// * config: payable, constant, and mutability description of the function.
    ///
    type EVMFunction =
        { name: string
          hash: EVMSelector
          inputs: EVMFunctionInputs
          outputs: EVMDatatype list
          //constant: string
          //payable: string
          config: StateMutability }


    ///
    /// Represents an event emitted by the EVM runtime during execution.
    /// Events may be listened for and acted upon.
    ///
    type EVMEvent =
        { name: string
          anonymous: bool
          inputs: EVMFunctionInputs
          hash: EVMSelector }


    ///
    /// Represents the Error function type of a Solidity contract.
    type EVMError =
        { name: string
          inputs: EVMFunctionInputs
          hash: EVMSelector }


    ///
    /// Supplied to the JsonValue parser. Emitted by the solc compiler and
    /// other tooling to describe a contract written in solidity.
    type ABI = ABI of string


    ///
    /// Represents the types of Solidity functions.
    type EVMFunctionTypes =
        | EVMFunction of EVMFunction
        | EVMEvent of EVMEvent
        | EVMError of EVMError


    ///
    /// Used during `makeEth_` calls to indicate the function being used in the call. `IndicatedFunction`
    /// is also returned from the function search helper.
    type FunctionIndicator =
        | IndicatedFunction of EVMFunction
        | ByString of string


    ///
    /// A type for allowing various types of criteria to be searched for in contract functions. The EVM allows function
    /// overloading, and thus some extra care may have to be taken by the user to ensure they are calling the function 
    /// they intend to. 
    type FunctionSearchTerm =
        | Name of string
        | SearchFunctionHash of EVMSelector
        | SearchFunctionInputs of EVMFunctionInputs
        //| SearchFunctionOutputs of EVMFunctionOutputs
        | SearchFunctionMutability of StateMutability


    ///
    /// 'Object' property of json output from `solc`.
    type RawContractBytecode = RawContractBytecode of string

    
    ///
    /// Convenience record that groups together various parameters that a user may wish to remain static between
    /// calls or txns.
    ///
    type ContractConstants =
        { address: EthAddress
          transactionType: string option
          maxFeePerGas: string option
          maxPriorityFeePerGas: string option
          data: EVMDatatype list option
          blockHeight: string option }
    
    
    ///
    /// Represents an undeployed contract and therefore doesn't have an address.
    /// May add in estimated gas to deploy and constructor arguments later.
    ///
    type UndeployedContract =
        { abi: ABI
          constructor: EVMSelector
          bytecode: RawContractBytecode
          chainId: string
          constructorArguments: EVMDatatype list option}


    ///
    /// Represents a deployed contract.
    type DeployedContract =
        { address: EthAddress
          abi: ABI
          functions: EVMFunction list
          events: EVMEvent list
          errors: EVMError list
          //deployedConstructorArguments: string // todo, probably involves some RPC calls to retrieve
          fallback: string
          receive: string
          chainId: string }
    
        
    ///
    /// Container for the result of attempting to load a contract from its ABI.
    type LoadContractResult = Result<DeployedContract, Web3Error>    

    
    