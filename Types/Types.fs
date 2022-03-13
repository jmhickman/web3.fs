namespace web3.fs
//
// Types for working with Ethereum connections
//

module Types =

    open FSharp.Data
    open FSharp.Json


    //
    // Misc types
    //


    let weiDiv = 1000000000000000000I


    //
    // Type Provider parser setup
    //


    // Experiment with making this one sample as inclusive as possible and see if it handles the minimal case(s) properly. Better to have just one.
    [<Literal>]
    let success = """{"jsonrpc":"2.0","id":67,"result":"g"}"""

    type RPCResponse = JsonProvider<success>

    [<Literal>]
    let sampleABI =
        """[{"type":"error","inputs": [{"name":"available","type":"uint256"},{"name":"required","type":"uint256"}],"name":"InsufficientBalance"}, {"type":"event","inputs": [{"name":"a","type":"uint256","indexed":true},{"name":"b","type":"bytes32","indexed":false}],"name":"Event"}, {"type":"event","inputs": [{"name":"a","type":"uint256","indexed":true},{"name":"b","type":"bytes32","indexed":false}],"name":"Event2"}, {"type":"function","inputs": [{"name":"a","type":"uint256"}],"name":"foo","outputs": []}]"""

    type ParsedABI = JsonProvider<sampleABI, SampleIsList=true>

    // found how to fix this, will address later
    //type RPCBlockResponse = JsonProvider<"./Samples/blocksample.json">


    //
    // Transaction Envelope types
    //


    let EARLIEST = "earliest"
    let LATEST = "latest"
    let PENDING = "pending"
    let ZERO = "0x0"

    // just aliases, may adapt validators later to emit stronger types
    type TxnType = string
    type Quantity = string
    type Data = string
    type EthAddress = string
    type AccessList = (EthAddress * Data list) list


    //
    // RPC Parameter Types
    //


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

    type EthParamAccounts = string list
    type EthParamBlockNumber = string list

    // There is a tremendous amount of leeway in acceptable RPC messages in terms of what is
    // included. This makes the type pretty gross-looking. data is the only properly 'required'
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


    //
    // RPC Method Types
    //


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
        | GetBlockbyNumber
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


    //
    // RPC Message type
    //


    type HttpRPCMessage =
        { method: RPCMethod
          paramlist: RPCParams }


    //
    // MailboxProcessor types
    //


    type MailboxChannel = ChannelMessageAndReply of HttpRPCMessage * AsyncReplyChannel<Result<RPCResponse.Root, string>>
    type HttpRPCMailbox = MailboxProcessor<MailboxChannel>


    //
    // Contract types
    //

    ///
    /// Set of tupled values created during Json parsing and filtering
    type IntermediateFunctionRepresentation =
        (option<JsonValue> * option<JsonValue> * option<JsonValue> * option<JsonValue>)


    ///
    /// Set of tupled values created during Json parsing and filtering
    type IntermediateEventRepresentation = (option<JsonValue> * option<JsonValue> * option<JsonValue>)


    ///
    /// Set of tupled values created during Json parsing and filtering
    type IntermediateErrorRepresentation = (option<JsonValue> * option<JsonValue>)


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
    /// Output of the process of extracting EVM function strings from Json
    type EVMFunctionInputs = EVMFunctionInputs of string


    ///
    /// Output of the process of extracting EVM function strings from Json
    type EVMFunctionOutputs = EVMFunctionOutputs of string


    ///
    /// The first 4 bytes of the Keccak256 hash of the function's canonical representation.
    type EVMSelector =
        | EVMFunctionHash of string
        | EVMEventSelector of string


    ///
    /// Describes the mutability of the function.
    /// Pure: No reads from blockchain state, no writes to blockchain state.
    /// View: Reads state, no writes to blockchain state.
    /// Nonpayable: Doesn't require an amount of ETH in the 'value' parameter of the txn object. Changes chain state.
    /// Payable: Accepts a value from the 'value' parameter of the txn object. Changes chain state.
    ///
    type StateMutability =
        | Pure
        | View
        | Nonpayable
        | Payable


    ///
    /// Represents a single function exposed by a Solidity contract.
    /// name: Name of the function from the source code
    /// hash: The 'function selector' hash, the Keccak256 hash of the 'canonical representation' of the function.
    /// config: payable, constant, and mutability description of the function.
    ///
    type EVMFunction =
        { name: string
          hash: EVMSelector
          inputs: EVMFunctionInputs
          outputs: EVMFunctionOutputs
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
    /// Represents an undeployed contract and therefore doesn't have an address.
    /// May add in estimated gas to deploy and constructor arguments later.
    ///
    type UndeployedContract =
        { abi: ABI
          functions: EVMFunction list
          events: EVMEvent list
          errors: EVMError list
          constructor: EVMFunction
          fallback: EVMFunction
          receive: EVMFunction
          bytecode: string } // might need some checking?


    ///
    /// Represents a deployed contract.
    type DeployedContract =
        { address: EthAddress
          abi: ABI
          functions: EVMFunction list
          events: EVMEvent list
          errors: EVMError list
          deployedConstructorArguments: string // todo, probably involves some RPC calls to retrieve
          fallback: string
          receive: string }


    ///
    /// Represents a failure of the JsonValue parser to consume the ABI.
    type ContractParseFailure = ContractParseFailure of string

    type LoadContractResult = Result<DeployedContract, ContractParseFailure>

    //
    /// Representations of EVM types in simplistic form. There is no effort made to 
    /// check that provided values of these types conform to any limitation of said
    /// types. 
    /// 
    /// **Warning** Note that while the Solidity documentation assert various `fixed`
    /// and `ufixed` types (describing very large floats with ranges of precision
    /// of whole and fractional parts), they are currently only partially 
    /// implemented in the EVM and so are unsupported here at this time.
    /// 
    type EVMDatatype =
    | Tuple of EVMDatatype list //
    | TupleArray of EVMDatatype list //
    | Address of string //
    | AddressArraySz of string list
    | AddressArray of string list //
    | Uint8 of string //
    | Uint32 of string //
    | Uint64 of string // 
    | Uint128 of string //
    | Uint256 of string //
    | Uint8ArraySz of string  list //
    | Uint32ArraySz of string  list //
    | Uint64ArraySz of string  list //
    | Uint128ArraySz of string  list //
    | Uint256ArraySz of string  list //
    | Uint8Array of string  list //
    | Uint32Array of string  list //
    | Uint64Array of string  list //
    | Uint128Array of string  list //
    | Uint256Array of string  list //
    | Int8 of string //
    | Int32 of string //
    | Int64 of string //
    | Int128 of string //
    | Int256 of string //
    | Int8ArraySz of string list //
    | Int32ArraySz of string list //
    | Int64ArraySz of string list //
    | Int128ArraySz of string list //
    | Int256ArraySz of string list //
    | Int8Array of string list //
    | Int32Array of string list //
    | Int64Array of string list //
    | Int128Array of string list //
    | Int256Array of string list //
    | Bool of bool //
    | BoolArraySz of bool list
    | BoolArray of bool list
    | BytesSz of string //
    | BytesSzArraySz of string list // 
    | BytesSzArray of string list // 
    | Bytes of string // 
    | BytesArraySz of EVMDatatype list // 
    | BytesArray of EVMDatatype list // 
    | Function of string //
    | FunctionArray of string list //
    | FunctionArraySz of string list //
    | String of string // 
    | StringArraySz of EVMDatatype list //
    | StringArray of EVMDatatype list //
    | Blob of string //