namespace web3.fs

open SHA3Core.Keccak

[<AutoOpen>]
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


    [<Literal>]
    let nullable =
        """[{"id":1,"jsonrpc":"2.0","result":{"blockHash":"0xc3646d4d8e3c650b15ef7f8a4d6d16fa4c7e68eb08195361182d7aa2eb3a0d65","blockNumber":"0x9dc3fe","contractAddress":null,"cumulativeGasUsed":"0x1166efb","effectiveGasPrice":"0x650fe5cd5","from":"0x2268b96e204379ee8366505c344ebe5cc34d3a46","gasUsed":"0x2e707","logs":[],"logsBloom":"0x00000000000000000000000000000000000000000000000000000000000000000000000000000","status":"0x1","to":null,"transactionHash":"0x0bd6acf13c1adf63c1f2b17ac1f9c4b98d94f2701728ffd5efe50aa77a6aa5aa","transactionIndex":"0x56","type":"0x2"}}, {"id":1,"jsonrpc":"2.0","result":null}, {"id":1,"jsonrpc":"2.0","error":{"message":"","code":-1}}]"""

    type RPCResponse = JsonProvider<nullable, SampleIsList=true>
    
    
    [<Literal>]
    let minedTransaction =
        """{"accessList":[],"blockHash":"0xc5430aaf3f85cb6b7d0400345d82bdd5ff3c16d230670827adefe024f2b84a19","blockNumber":"0x9ee268","chainId":"0x4","from":"0x2268b96e204379ee8366505c344ebe5cc34d3a46","gas":"0x5566","gasPrice":"0x700f2328","hash":"0xf7d92d8090c1a6f6f811c07ef2b98b044304545c65af57fb6424f33d59ecd1ce","input":"0x91fc651700000000000000000000000000000000000000000000000000000000000000ff","maxFeePerGas":"0x7f541fb5","maxPriorityFeePerGas":"0x41a53453","nonce":"0x32","r":"0x4f9f9a8f18b3756e647816d16e240588d2cc7f212d4fbdcc7871c37327dd300d","s":"0x91fd6da320a1a006aad57bce8e6f062a6caf68066966488039d791d7f8b1ae3","to":"0x894113aa49fe903be4c7b8fdddacf503fa88c1f7","transactionIndex":"0x11","type":"0x2","v":"0x0","value":"0x0"}"""
    
    type RPCMinedTransaction = JsonProvider<minedTransaction>
    
    [<Literal>]
    let block =
        """{"author":"0x0000000000000000000000000000000000000000","baseFeePerGas":"0xb","difficulty":"0x1","extraData":"0x696e667572612e6c601","gasLimit":"0x1c9c364","gasUsed":"0xd8ec3e","hash":"0x5fe3bc231d6b492665af141fd32aaffbf2691cb52a5d4eb9de7e340a48cb8071","logsBloom":"0x2c20c05c100002109041","miner":"0x0000000000000000000000000000000000000000","number":"0xa01b7b","parentHash":"0xf6b6bae73e27c3be6a96ed47bb092cb3c5152eec7f55c165d39ccd1a977bfef0","receiptsRoot":"0x6c2810d1e1356e480e000d967bd718bbd84bb74739b0ffe7dabafc592ef22589","sealFields":["0xa000000000000000000000000","0x880000000000000000"],"sha3Uncles":"0x1dcc4de8da142fd40d49347","size":"0xd435","stateRoot":"0x06ee90180a5a638bed","timestamp":"0x6255bd2e","totalDifficulty":"0x1097454","transactions":["0xca8148612","0x791f583b1e5ee","0xadc458105cf51","0x3b6940a81642e9d9","0x3cdf20aac"],"transactionsRoot":"0x981b9371a29263153b8","uncles":[]}"""

    type RPCBlock = JsonProvider<block>    
        
    [<Literal>]
    let sampleABI =
        """[{"type":"error","inputs": [{"name":"available","type":"uint256"},{"name":"required","type":"uint256"}],"name":"InsufficientBalance"}, {"type":"event","inputs": [{"name":"a","type":"uint256","indexed":true},{"name":"b","type":"bytes32","indexed":false}],"name":"Event"}, {"type":"event","inputs": [{"name":"a","type":"uint256","indexed":true},{"name":"b","type":"bytes32","indexed":false}],"name":"Event2"}, {"type":"function","inputs": [{"name":"a","type":"uint256"}],"name":"foo","outputs": []}]"""

    type internal ParsedABI = JsonProvider<sampleABI, SampleIsList=true>

    ///
    /// Derived from Solc output, remix output looks different
    [<Literal>]
    let sampleBytecode =
        """{"contractName":"UnitTestingContract","abi":[],"metadata":"","bytecode":"6080604052348015610010572083870101525b50601f01601f19169290920160200192915050565b6001600160a01b0391909116815260200190565b901515815260200190565b6001600160801b031991909116815260200190565b60006020825261181360208301846119fb565b90815260200190565b60009190910b815260200190565b60208082526025908201527f596f7520617265206e6f7420746865206f776e6572206f662074686520636f6e6040820152641d1c9858dd60da1b606082015260800190565b60ff91909116815260200190565b6000808335601e19843603018112611b0d578283fd5b83018035915067ffffffffffffffff821115611b27578283fd5b60200191503681900382131561179457600080fd5b6000808335601e19843603018112611b0d578182fd5b600281046001821680611b6657607f821691505b60208210811415611b8757634e487b7160e01b600052602260045260246000fd5b50919050565b6000600019821415611bad57634e487b7160e01b81526011600452602481fd5b506001019056fea2646970667358221220763f965497247c8646c2fd9ef88a4553d300863a6d99a00843155c1c7086d7ba64736f6c63430008000033","deployedBytecode":"6080604052600436106102cd5760003560e01c80638820238c11610175578063cc4dd74a116100dc578063e7a96f6d11610095578063f9cceccc1161006f578f91909116815260200190565b6000808335601e19843603018112611b0d578283fd5b83018035915067ffffffffffffffff821115611b27578283fd5b60200191503681900382131561179457600080fd5b6000808335601e19843603018112611b0d578182fd5b600281046001821680611b6657607f821691505b60208210811415611b8757634e487b7160e01b600052602260045260246000fd5b50919050565b6000600019821415611bad57634e487b7160e01b81526011600452602481fd5b506001019056fea2646970667358221220763f965497247c8646c2fd9ef88a4553d300863a6d99a00843155c1c7086d7ba64736f6c63430008000033","sourceMap":"65:4923:0:-:0;;;3:0;;;;;;","deployedSourceMap":"65:4923:0:-:0;;;;;;:::i;:::-;;;;;;;:::i;1110:99::-;;;;;;;;;;;;;:::i;190:26::-;;;;;;;;;;-1:-1:-1;190:26:0;;;;;:::i;:::-;;:::i;","sourcePath":"c:/Users/jon_h/source/repos/UnitTestingContract/UnitTestingContract.sol","compiler":{"name":"solc","version":"0.8.0+commit.c7dfd78e"},"ast":{},"functionHashes":{"aBool()":"350ca843"},"gasEstimates":{"creation":{"codeDepositCost":"1429200","executionCost":"infinite","totalCost":"infinite"},"external":{}}}"""
   
    type internal ContractBytecode = JsonProvider<sampleBytecode>
    
        
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Constants
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    let EARLIEST = "earliest"
    let LATEST = "latest"
    let PENDING = "pending"
    let ZERO = "0x0"
    let ZEROV = "0"
    let fakedOffset =  "0000000000000000000000000000000000000000000000000000000000000020"
    let zeroEVMValue = "0000000000000000000000000000000000000000000000000000000000000000"
    let nullAddress = "0000000000000000000000000000000000000000"
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Chains
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    let ETHEREUM_MAINNET = "0x01"
    let RINKEBY = "0x04"
    
    
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
        | Tuple of EVMDatatype list
        | TupleArray of EVMDatatype list
        | TupleArraySz of EVMDatatype list 
        | Address of string
        | AddressArraySz of string list
        | AddressArray of string list
        | Uint256 of string
        | Uint256ArraySz of string list
        | Uint256Array of string list
        | Int256 of string
        | Int256ArraySz of string list 
        | Int256Array of string list
        | Bool of bool
        | BoolArraySz of bool list
        | BoolArray of bool list
        | BytesSz of string
        | BytesSzArraySz of string list 
        | BytesSzArray of string list 
        | Bytes of string
        | BytesArraySz of EVMDatatype list
        | BytesArray of EVMDatatype list
        | Function of string
        | FunctionArray of string list
        | FunctionArraySz of string list
        | String of string
        | StringArraySz of EVMDatatype list
        | StringArray of EVMDatatype list
        | Blob of string

    
    ///
    /// Indicators used in `checkBounds` to signal the intended size of the numeric type.
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
        
    type CheckEVMData = CheckedSuccess
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Data Types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
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
    // Eth Parameter Types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // There is a tremendous amount of leeway in acceptable RPC messages in terms of what is
    // included. This makes the type pretty gross-looking. `data` is the only properly 'required'
    // value, and in practice toAddr and from will be present as well (but not always!)
    type EthParam1559Call =
        { [<JsonField("type")>] txnType: TxnType option // Seems optional, needs research.
          nonce: Quantity option // Missing nonce is fine, if talking to a wallet
          [<JsonField("to")>] toAddr: EthAddress option // Missing to is fine, if deploying a contract
          from: EthAddress option // Missing 'from' is not great, may cause errors if wallet or node can't determine sender
          gas: Quantity option // Missing gas limit is fine, if talking to wallet
          value: Quantity option // Missing value is fine, if making a Call. May still be fine if txn isn't payable
          data: Data // Calls, txn and deploys should always have a valid data value
          maxPriorityFeePerGas: Quantity option // Missing gas is fine, if talking to wallet
          maxFeePerGas: Quantity option // Missing gas is fine, if talking to wallet
          accessList: AccessList option // Typically empty
          chainId: Quantity option } // Seems to be genuinely optional at this time, but should be included.

    
    type EthParam1559OverrideCall =
        { [<JsonField("type")>] txnType: TxnType option // Seems optional, needs research.
          nonce: Quantity option // Missing nonce is fine, if talking to a wallet
          [<JsonField("to")>] toAddr: EthAddress option // Missing to is fine, if deploying a contract
          from: EthAddress option // Missing 'from' is not great, may cause errors if wallet or node can't determine sender
          gas: Quantity option // Missing gas limit is fine, if talking to wallet
          value: Quantity option // Missing value is fine, if making a Call. May still be fine if txn isn't payable
          data: Data // Calls, txn and deploys should always have a valid data value
          maxPriorityFeePerGas: Quantity option // Missing gas is fine, if talking to wallet
          maxFeePerGas: Quantity option // Missing gas is fine, if talking to wallet
          accessList: AccessList option // Typically empty
          chainId: Quantity option
          bytecode: string
          fakeBalance: Quantity option
          fakeNonce: Quantity option
          fakeState: string option
          fakeStateDiff: string option }
        
    type EthParam2930CreateAccessList = EthParam1559Call
    
    type EthParamGetLogs =
        { fromBlock: string option
          toBlock: string option
          address: string option
          topics: string array option
          blockhash: string option }
    
    
    type EthParamNewFilter =
        { fromBlock: string option
          toBlock: string option
          address: string option
          topics: string array option }
    
    type EthGenericRPC = string list
    
    type EthParam1559EstimateGas = EthParam1559Call
    type EthParam1559SendTransaction = EthParam1559Call
    type EthParam1559SignTransaction = EthParam1559Call
    

    type EthParam =
        | EthGenericRPC of EthGenericRPC
        | EthParam1559Call of EthParam1559Call
        | EthParam1559EstimateGas of EthParam1559EstimateGas
        | EthParam1559SignTransaction of EthParam1559SignTransaction
        | EthParam1559SendTransaction of EthParam1559Call
        

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Eth Method Types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    [<RequireQualifiedAccess>]
    type EthMethod =
        | Accounts
        | BlockNumber
        | Call
        | Coinbase
        | ChainId
        | EstimateGas
        | GasPrice
        | GetBalance
        | GetBlockByHash
        | GetBlockByNumber
        | GetBlockTransactionCountByHash
        | GetBlockTransactionCountByNumber
        | GetCode
        //| GetFilterChanges
        //| GetFilterLogs
        //| GetLogs
        | GetStorageAt
        | GetTransactionCount
        | GetTransactionByHash
        | GetTransactionByBlockHashAndIndex
        | GetTransactionByBlockNumberAndIndex
        | GetTransactionReceipt
        | GetUncleCountByBlockHash
        | GetUncleCountByBlockNumber
        //| NewFilter
        //| NewBlockFilter
        //| NewPendingTransactionFilter
        | ProtocolVersion
        | Syncing
        | SendTransaction
        | SendRawTransaction
        | Sign
        | SignTransaction
        //| UninstallFilter

    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // RPC Message Types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    type HttpRPCMessage =
        { method: EthMethod
          paramList: EthParam
          blockHeight: string }

    
    ///
    /// Record representing a transaction receipt object from the RPC node.
    type TransactionReceipt =
        { blockHash: string
          blockNumber: string
          contractAddress: EthAddress option
          cumulativeGasUsed: string
          effectiveGasPrice: string
          from: EthAddress
          gasUsed: string
          logs: string list
          logsBloom: string
          status: string
          toAddr: EthAddress option
          transactionHash: EthTransactionHash
          transactionIndex: string
          tType: string }
    
    
    let nullTransactionReceipt =
        { blockHash = "wrong unwrap or upstream web3 error"
          blockNumber = "wrong unwrap or upstream web3 error"
          contractAddress = None
          cumulativeGasUsed = "wrong unwrap or upstream web3 error"
          effectiveGasPrice = "wrong unwrap or upstream web3 error"
          from = zeroEVMValue |> EthAddress
          gasUsed = "wrong unwrap or upstream web3 error"
          logs = ["wrong unwrap or upstream web3 error"]
          logsBloom = "wrong unwrap or upstream web3 error"
          status = "wrong unwrap or upstream web3 error"
          toAddr = None
          transactionHash = zeroEVMValue |> EthTransactionHash
          transactionIndex = "wrong unwrap or upstream web3 error"
          tType = "wrong unwrap or upstream web3 error" }
    
    ///
    /// Record representing a block on the Ethereum blockchain 
    type EthBlock =
        { author: string
          baseFeePerGas: string
          difficulty: string
          extraData: string
          gasLimit: string
          gasUsed: string
          hash: EthTransactionHash
          logsBloom: string
          miner: EthAddress
          number: string
          parentHash: string
          receiptsRoot: string
          sealFields: string list
          sha3Uncles: string
          size: string
          stateRoot: string
          timestamp: string
          totalDifficulty: string
          transactions: string list
          transactionsRoot: string
          uncles: string list }
        
    
    ///
    /// Record representing a block on the Ethereum blockchain 
    let nullEthBlock =
        { author = "wrong unwrap or upstream web3 error"
          baseFeePerGas = "wrong unwrap or upstream web3 error"
          difficulty = "wrong unwrap or upstream web3 error"
          extraData = "wrong unwrap or upstream web3 error"
          gasLimit = "wrong unwrap or upstream web3 error"
          gasUsed = "wrong unwrap or upstream web3 error"
          hash = "wrong unwrap or upstream web3 error" |> EthTransactionHash
          logsBloom = "wrong unwrap or upstream web3 error"
          miner = zeroEVMValue |> EthAddress
          number = "wrong unwrap or upstream web3 error"
          parentHash = "wrong unwrap or upstream web3 error"
          receiptsRoot = "wrong unwrap or upstream web3 error"
          sealFields = ["wrong unwrap or upstream web3 error"]
          sha3Uncles = "wrong unwrap or upstream web3 error"
          size = "wrong unwrap or upstream web3 error"
          stateRoot = "wrong unwrap or upstream web3 error"
          timestamp = "wrong unwrap or upstream web3 error"
          totalDifficulty = "wrong unwrap or upstream web3 error"
          transactions = ["wrong unwrap or upstream web3 error"]
          transactionsRoot = "wrong unwrap or upstream web3 error"
          uncles = ["wrong unwrap or upstream web3 error"] }
    
    
    ///
    ///  Record representing a completed transaction on the Ethereum network.
    type MinedTransaction =
          { accessList: string list
            blockHash: string
            blockNumber: string
            chainId: string
            from: EthAddress
            gas: string
            gasPrice: string
            hash: EthTransactionHash
            input: string
            maxFeePerGas: string
            maxPriorityFeePerGas: string
            nonce: string
            r: string
            s: string
            toAddr: EthAddress
            transactionIndex: string
            tType: string
            v: string
            value: string }
    
    
    let nullMinedTransaction =
          { accessList = ["wrong unwrap or upstream web3 error"]
            blockHash = "wrong unwrap or upstream web3 error"
            blockNumber = "wrong unwrap or upstream web3 error"
            chainId = "wrong unwrap or upstream web3 error"
            from = zeroEVMValue |> EthAddress
            gas = "wrong unwrap or upstream web3 error"
            gasPrice = "wrong unwrap or upstream web3 error"
            hash = zeroEVMValue |> EthTransactionHash
            input = "wrong unwrap or upstream web3 error"
            maxFeePerGas = "wrong unwrap or upstream web3 error"
            maxPriorityFeePerGas = "wrong unwrap or upstream web3 error"
            nonce = "wrong unwrap or upstream web3 error"
            r = "wrong unwrap or upstream web3 error"
            s = "wrong unwrap or upstream web3 error"
            toAddr = zeroEVMValue |> EthAddress
            transactionIndex = "wrong unwrap or upstream web3 error"
            tType = "wrong unwrap or upstream web3 error"
            v = "wrong unwrap or upstream web3 error"
            value = "wrong unwrap or upstream web3 error" }
          
    
    ///
    /// Overall type for errors in various places in the pipeline. Not final at all.
    type Web3Error =
        | ContractParseFailure of string
        | ConnectionError of string
        | DataValidatorError of string
        | HttpClientError of string
        | RPCResponseError of string
        | EthCallIntoNonCallPipeline
        | RPCNullResponse
        | ConstructorArgumentsToEmptyConstructorError
        | ConstructorArgumentsMissingError
        | ValueToNonPayableFunctionError
        | EthAddressError
        
        
    ///
    /// Union of potential responses from the EVM through an RPC node. Null here is a 'valid' result, usually indicating
    /// that a transaction doesn't exist at a particular hash, or that a transaction hasn't been included in the chain
    /// yet. 
    type CallResponses =
        | SimpleValue of string //
        | Block of EthBlock //
        | TransactionHash of EthTransactionHash // 
        | TransactionReceiptResult of TransactionReceipt //
        | Transaction of MinedTransaction //
        | CallResult of EVMDatatype list //
        | Empty //
    
    
    ///
    /// Provides signals for the logger 
    type LogSignal =
        | Log
        | Emit
        | LogAndEmit
        | Quiet
    
    
    ///
    /// Signals to estimateGas how to render the output
    type GasUnits =
        | HexGas
        | DecimalGas
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // MailboxProcessor types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Convenience type
    type Web3Connection = HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>

    
    ///
    /// MailboxProcessor for sending and receiving the results of an RPC transmission
    type MailboxTransaction =
        TransactionMessageAndReply of HttpRPCMessage * AsyncReplyChannel<Result<RPCResponse.Root, Web3Error>>
    
    
    ///
    /// Convenience type
    type HttpRPCMailbox = MailboxProcessor<MailboxTransaction>

    
    ///
    /// MailboxProcessor for monitoring pending transactions
    type MailboxReceiptManager =
        ReceiptMessageAndReply of EthTransactionHash * AsyncReplyChannel<Result<CallResponses, Web3Error>>
    
    ///
    /// Convenience type
    type ReceiptManagerMailbox = MailboxProcessor<MailboxReceiptManager>

    ///
    /// Convenience type
    type Monitor = EthTransactionHash -> Result<CallResponses, Web3Error>
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Contract types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Set of tupled values created during Json parsing and filtering
    type internal IntermediateFunctionRepresentation =
        option<JsonValue> * option<JsonValue> * option<JsonValue> * option<JsonValue>


    ///
    /// Set of tupled values created during Json parsing and filtering
    type internal IntermediateEventRepresentation = option<JsonValue> * option<JsonValue> * option<JsonValue>


    ///
    /// Set of tupled values created during Json parsing and filtering
    type internal IntermediateErrorRepresentation = option<JsonValue> * option<JsonValue>


    ///
    /// Result of the 'flattening' of the JSON representation of an EVM function. Used as the input to
    /// the Keccak (SHA3) hash generated to provide the 'function selector' of a function and
    /// only relevant for that purpose.
    ///
    type CanonicalRepresentation =
        internal
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
          canonicalInputs: EVMFunctionInputs
          internalOutputs: EVMDatatype list
          canonicalOutputs: EVMFunctionOutputs
          config: StateMutability
          // chainId: string
          // address: EthAddress
          }


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
        | Receive
        | Fallback
        


    ///
    /// A type for allowing various types of criteria to be searched for in contract functions. The EVM allows function
    /// overloading, and thus some extra care may have to be taken by the user to ensure they are calling the function 
    /// they intend to. 
    type FunctionSearchTerm =
        | Name of string
        | SearchFunctionHash of EVMSelector
        | SearchFunctionInputs of EVMFunctionInputs
        | SearchFunctionOutputs of EVMFunctionOutputs
        | SearchFunctionMutability of StateMutability


    ///
    /// 'Object' property of json output from `solc`.
    type RawContractBytecode = RawContractBytecode of string

    
    ///
    /// Convenience record that groups together various parameters that a user may wish to remain static between
    /// calls or txns.
    ///
    type ContractConstants =
        { walletAddress: EthAddress
          transactionType: string option
          maxFeePerGas: string option
          maxPriorityFeePerGas: string option
          data: EVMDatatype list option
          blockHeight: string option
          defaultValue: string option }
    
    
    ///
    /// Represents an undeployed contract and therefore doesn't have an address.
    /// May add in estimated gas to deploy and constructor arguments later.
    ///
    type UndeployedContract =
        { abi: ABI
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
          //deployedConstructorArguments: string // todo maybe? probably involves some reasonable work to retrieve generically
          chainId: string }
    
    
    ///
    /// Web3Environment is a convenience grouping of necessary functions and data to perform operations with web3.fs.  
    type Web3Environment =
        { connection: Web3Connection
          monitor: Monitor
          constants: ContractConstants
          digest: Keccak}