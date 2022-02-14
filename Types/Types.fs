namespace web3.fs
///
/// Types for working with Ethereum connections
///

module Types =

    open FSharp.Data
    open FSharp.Json

    ///
    /// Misc types
    ///

    let weiDiv = 1000000000000000000I

    ///
    /// Type Provider parser setup
    ///

    // Experiment with making this one sample as inclusive as possible and see if it handles the minimal case(s) properly. Better to have just one.
    [<Literal>]
    let success = """{"jsonrpc":"2.0","id":67,"result":"g"}"""

    type RPCResponse = JsonProvider<success>

    //type RPCBlockResponse = JsonProvider<"./Samples/blocksample.json">


    ///
    /// Transaction Envelope types
    ///

    let EARLIEST = "earliest"
    let LATEST = "latest"
    let PENDING = "pending"
    let ZERO = "0x0"

    //
    type TxnType = string
    type Quantity = string
    type Data = string
    type Address = string
    type AccessList = (Address * Data list) list

    ///
    /// RPC Parameter Types
    ///

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

    // Many of these values are listed as Options, but in practice will almost never be 'None.'
    // This is just laziness and not wanting to write two different verification functions for
    // optioned vs non-optioned types.
    type EthParam1559Call =
        { [<JsonField("type")>]
          txnType: TxnType option // Seems optional, needs research.
          nonce: Quantity option // Missing nonce is fine, if talking to a wallet
          [<JsonField("to")>]
          toAddr: Address option // Missing to is fine, if deploying a contract
          from: Address option // Missing 'from' is not great, may cause errors if wallet or node can't determine sender
          gas: Quantity option // Missing gas is fine, if talking to wallet
          value: Quantity option // Missing value is fine, if making a Call. May still be fine if txn isn't payble
          data: Data // Calls, txn and deploys should always have a data value
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

    ///
    /// RPC Method Types
    ///

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

    ///
    /// RPC Message type
    ///

    type HttpRPCMessage =
        { method: RPCMethod
          paramlist: RPCParams }

    ///
    /// MailboxProcessor types
    ///

    type MailboxChannel = ChannelMessageAndReply of HttpRPCMessage * AsyncReplyChannel<Result<RPCResponse.Root, string>>
    type HttpRPCMailbox = MailboxProcessor<MailboxChannel>
