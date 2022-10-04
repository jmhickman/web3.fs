namespace Web3.fs

[<AutoOpen>]
module Types =
    open System
    
    open FSharp.Data
    open FSharp.Json


    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Misc types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    /// Quantity of value values
    let weiFactor = 1000000000000000000I // one Ether as wei
    let gweiFactor = 1000000000I // as wei 
                     
    type Wei = string
    type Gwei = string
    type Ether = string
    
    type WeiConversion =
        | Wei of Wei
        | Gwei of Gwei
        | Ether of Ether
        
    
    type ChainId = string
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Type Provider parser setup
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    type RPCResponse = JsonProvider<"samples/nullable.json", EmbeddedResource="Web3.fs, Web3.fs.samples.nullable.json", SampleIsList=true>
    
    type RPCMinedTransaction = JsonProvider<"samples/sampleTransaction.json", EmbeddedResource="Web3.fs, Web3.fs.samples.sampleTransaction.json">
    
    type RPCBlock = JsonProvider<"samples/blocksample.json", EmbeddedResource="Web3.fs, Web3.fs.samples.blocksample.json">    
        
    type internal ParsedABI = JsonProvider<"samples/sampleABI.json", EmbeddedResource="Web3.fs, Web3.fs.samples.sampleABI.json", SampleIsList=true>

    type internal ContractBytecode = JsonProvider<"samples/sampleSolcBytecode.json", EmbeddedResource="Web3.fs, Web3.fs.samples.sampleSolcBytecode.json">
    
    type internal RemixBytecode = JsonProvider<"samples/sampleRemixBytecode.json", EmbeddedResource="Web3.fs, Web3.fs.samples.sampleRemixBytecode.json">
        
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Constants
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    let public EARLIEST = "earliest"
    let public LATEST = "latest"
    let public PENDING = "pending"
    let public ZEROHEX = "0x0"
    let public ZEROVALUE = "0"
    let internal fakedOffset =  "0000000000000000000000000000000000000000000000000000000000000020"
    let public zeroEVMValue = "0000000000000000000000000000000000000000000000000000000000000000"
    let internal nullAddress = "0000000000000000000000000000000000000000"
    let internal QUOTE = '"' |> fun s -> s.ToString()
    let internal EMPTY = ""
    let internal ENSZero = Convert.FromHexString(zeroEVMValue)
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Chains
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Convenience bindings for common networks. These are not assertions of
    /// compatibility.
    /// 
    let public ETHEREUM_MAINNET = "0x1"
    let public ROPSTEN = "0x3"
    let public RINKEBY = "0x4"
    let public GORLI = "0x5"
    let public ETHCLASSIC = "0x6"
    let public OPTIMISM = "0xa"
    let public KOVAN = "0x2a"
    let public ARBITRUM = "0xa4b1"
    let public MOONRIVER = "0x505"
    let public MOONBEAM = "0x504"
    let public GANACHE = "0x539"
    
    
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // EVM data types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    ///
    /// Used in revised type system. Acts as a flag for a set of functions to
    /// use to test that inputs don't exceed the indicated width.
    type Bitness =
        | B8
        | B16
        | B24
        | B32
        | B40
        | B48
        | B56
        | B64
        | B72
        | B80
        | B88
        | B96
        | B104
        | B112
        | B120
        | B128
        | B136
        | B144
        | B152
        | B160
        | B168
        | B176
        | B184
        | B192
        | B200
        | B208
        | B216
        | B224
        | B232
        | B240
        | B248
        | B256
    
    
    ///
    /// Used in revised type system. Acts as a flag for a set of functions to
    /// use to test that inputs don't exceed the indicated length. 
    type ByteLength =
        | L1
        | L2
        | L3
        | L4
        | L5
        | L6
        | L7
        | L8
        | L9
        | L10
        | L11
        | L12
        | L13
        | L14
        | L15
        | L16
        | L17
        | L18
        | L19
        | L20
        | L21
        | L22
        | L23
        | L24
        | L25
        | L26
        | L27
        | L28
        | L29
        | L30
        | L31
        | L32     
        

    /// Representations of EVM types in simplistic form. 
    ///
    /// **Warning** Note that while the Solidity documentation assert various
    /// `fixed` and `ufixed` types (describing very large floats with ranges of
    /// precision for the whole and fractional parts), they are currently only 
    /// partially implemented in the ABI encoder and so are unsupported here at
    /// this time.
    ///
    /// **Warning** There is no attempt to create types for multidimensional
    /// arrays. 
    ///
    type EVMDatatype =
        | Tuple of EVMDatatype list
        | TupleArray of EVMDatatype list
        | TupleArraySz of EVMDatatype list 
        | Address of string
        | AddressArraySz of string list
        | AddressArray of string list
        | Bool of bool
        | BoolArraySz of bool list
        | BoolArray of bool list
        | Bytes of string
        | BytesArraySz of EVMDatatype list
        | BytesArray of EVMDatatype list
        | String of string
        | StringArraySz of EVMDatatype list
        | StringArray of EVMDatatype list
        | Blob of string
        | Int of Bitness * string
        | IntArraySz of Bitness * string list
        | IntArray of Bitness * string list
        | Uint of Bitness * string
        | UintArraySz of Bitness * string list
        | UintArray of Bitness * string list
        | BytesN of ByteLength * string
        | BytesNArraySz of ByteLength * string list
        | BytesNArray of ByteLength * string list
//        | Function of string
//        | FunctionArray of string list
//        | FunctionArraySz of string list
    
    
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
    /// A 'raw' representation of an EIP-1559 compliant txn object. This is fed
    /// through a validation function only to ensure that the bytes meet certain
    /// criteria (documented in Common.fs)
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

    /// There is a tremendous amount of leeway in acceptable RPC messages in
    /// terms of what is included. This makes the type pretty gross-looking.
    /// `data` is the only properly 'required' value, and in practice toAddr and
    /// from will be present as well (but not always!). `data` will also be
    /// intentionally '0x' when a contract's `receive()` is called.
    type EthParam1559Call =
        { [<JsonField("type")>] txnType: TxnType option
          nonce: Quantity option
          [<JsonField("to")>] toAddr: EthAddress option 
          from: EthAddress option 
          gas: Quantity option 
          value: Quantity option 
          data: Data 
          maxPriorityFeePerGas: Quantity option 
          maxFeePerGas: Quantity option 
          accessList: AccessList option 
          chainId: Quantity option }

    
    // Potential future compat with override calls, not in use currently.
    (*
    type EthParam1559OverrideCall =
        { [<JsonField("type")>] txnType: TxnType option 
          nonce: Quantity option 
          [<JsonField("to")>] toAddr: EthAddress option 
          from: EthAddress option 
          gas: Quantity option
          value: Quantity option 
          data: Data 
          maxPriorityFeePerGas: Quantity option 
          maxFeePerGas: Quantity option 
          accessList: AccessList option
          chainId: Quantity option
          bytecode: string
          fakeBalance: Quantity option
          fakeNonce: Quantity option
          fakeState: string option
          fakeStateDiff: string option }
    *)

    //type EthParam2930CreateAccessList = EthParam1559Call
    
    (*
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
    *)

    
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

    
    /// Basic message container for RPC MailboxProcessor
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
        { baseFeePerGas: string // gold
          difficulty: string
          extraData: string
          gasLimit: string // gold
          gasUsed: string // gold
          hash: EthTransactionHash // blue
          logsBloom: string
          miner: EthAddress // blue
          number: string //blue
          parentHash: string
          receiptsRoot: string
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
        { baseFeePerGas = "wrong unwrap or upstream web3 error"
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
        { blockHash: string 
          blockNumber: string 
          from: EthAddress
          gas: string 
          gasPrice: string 
          hash: EthTransactionHash 
          input: string
          nonce: string 
          r: string 
          s: string 
          toAddr: EthAddress option 
          transactionIndex: string 
          tType: string 
          v: string 
          value: string } 
    
    
    let nullMinedTransaction =
        { blockHash = "wrong unwrap or upstream web3 error"
          blockNumber = "wrong unwrap or upstream web3 error"
          from = zeroEVMValue |> EthAddress
          gas = "wrong unwrap or upstream web3 error"
          gasPrice = "wrong unwrap or upstream web3 error"
          hash = zeroEVMValue |> EthTransactionHash
          input = "wrong unwrap or upstream web3 error"
          nonce = "wrong unwrap or upstream web3 error"
          r = "wrong unwrap or upstream web3 error"
          s = "wrong unwrap or upstream web3 error"
          toAddr = zeroEVMValue |> EthAddress |> Some
          transactionIndex = "wrong unwrap or upstream web3 error"
          tType = "wrong unwrap or upstream web3 error"
          v = "wrong unwrap or upstream web3 error"
          value = "wrong unwrap or upstream web3 error" }
          
    
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
    /// Result of the 'flattening' of the JSON representation of an EVM
    /// function. Used as the input to the Keccak hash generator to provide the
    /// 'function selector' of a function.
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
    /// The first 4 bytes of the Keccak256 hash of the function's canonical
    /// representation.
    /// 
    type EVMFunctionHash = string
    type EVMEventHash = string

    
    ///
    /// Describes the mutability of the function.
    /// * Pure: No reads from blockchain state, no writes to blockchain state.
    /// * View: Reads state, no writes to blockchain state.
    /// * Nonpayable: Doesn't require an amount of ETH in the 'value' parameter
    ///     of the txn object. Changes chain state.
    /// * Payable: Accepts a value from the 'value' parameter of the txn object.
    ///     Changes chain state.
    ///
    type StateMutability =
        | Pure
        | View
        | Nonpayable
        | Payable


    ///
    /// Represents a single function exposed by a Solidity contract.
    /// * name: Name of the function from the source code
    /// * hash: The 'function selector' hash, the Keccak256 hash of the
    ///     'canonical representation' of the function.
    /// * canonicalInputs: A string representation of the function's inputs,
    ///     like "(address)".
    /// * internalOutputs: Web3.fs' model of the output types.
    /// * canonicalOutputs: A string representation of the function's outputs,
    ///     like "(uint256)[]"
    /// * config: The function's state mutability
    ///
    type EVMFunction =
        { name: string
          hash: EVMFunctionHash
          canonicalInputs: EVMFunctionInputs
          internalOutputs: EVMDatatype list
          canonicalOutputs: EVMFunctionOutputs
          config: StateMutability }


    ///
    /// Represents an event emitted by the EVM runtime during execution.
    /// Events may be listened for and acted upon.
    ///
    type EVMEvent =
        { name: string
          anonymous: bool
          inputs: EVMFunctionInputs
          hash: EVMEventHash }


    ///
    /// Represents the Error function type of a Solidity contract.
    type EVMError =
        { name: string
          inputs: EVMFunctionInputs
          hash: EVMFunctionHash }


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
    /// Indicates the function being used in a call. `IndicatedFunction` is also
    /// returned from the function search helper.
    type FunctionIndicator =
        | IndicatedFunction of EVMFunction
        | Receive
        | Fallback
        


    ///
    /// A type for allowing various types of criteria to be searched for in
    /// contract functions. The EVM allows function overloading, and thus some
    /// extra care may have to be taken by the user to ensure they are calling
    /// the function they intend to. 
    type FunctionSelector =
        | ByName of string
        | ByNameAndHash of (string * EVMFunctionHash)
        | ByNameAndInputs of (string * EVMFunctionInputs)
        | ByNameAndOutputs of (string * EVMFunctionOutputs)
        | ByNameAndMutability of (string * StateMutability)
        | Receive
        | Fallback


    ///
    /// 'Object' property of json output from `solc`.
    type RawContractBytecode = RawContractBytecode of string

    
    ///
    /// Convenience record that groups together various parameters that a user
    /// may wish to remain static between calls or txns.
    ///
    type ContractConstants =
        { walletAddress: EthAddress
          transactionType: string option
          maxFeePerGas: string option
          maxPriorityFeePerGas: string option
          arguments: EVMDatatype list option
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
          constructorArguments: EVMDatatype list
          stateMutability: StateMutability }


    ///
    /// Represents a deployed contract.
    type DeployedContract =
        { address: EthAddress
          abi: ABI
          chainId: string 
          functions: EVMFunction list
          events: EVMEvent list
          errors: EVMError list
          hasFallback: bool
          hasReceive: bool }
    
    
    let dummyTransaction =
        { utxnType = "0x2"
          unonce = ""
          utoAddr = ""
          ufrom = ""
          ugas = ""
          uvalue = ""
          udata = "0x"
          umaxFeePerGas = "" 
          umaxPriorityFeePerGas = "" 
          uaccessList = []
          uchainId = "" }
    
    
    
    ///
    /// Overall type for errors in various places in the pipeline.
    type Web3Error =
        | ContractParseFailure of string
        | ConnectionError of string
        | DataValidatorError of string
        | HttpClientError of string
        | RPCResponseError of string
        | PayableFunctionZeroValueWarning of string
        | WrongChainInSignerError
        | ContractABIContainsHashCollisionsError
        | EthCallIntoNonCallPipelineError
        | RPCNullResponse
        | EmptyBytecodeError
        | ConstructorArgumentsToEmptyConstructorError
        | ConstructorArgumentsMissingError
        | ArgumentsToEmptyFunctionSignatureError
        | ContractLacksFallbackError
        | ContractLacksReceiveError
        | FunctionNotFoundError
        | AmbiguousFunctionError of FunctionIndicator list
        | FunctionArgumentsMissingError
        | FunctionArgumentsValidationError of string
        | InvalidValueArgumentError
        | ValueToNonPayableFunctionError
        | EthAddressError
        | GenericPipelineError of string
        
        
        
    ///
    /// Union of potential responses from the EVM through an RPC node. `Empty`
    /// here is a 'valid' result, usually indicating that a transaction doesn't
    /// exist at a particular hash, or that a transaction hasn't been included
    /// in the chain yet.
    /// 
    type CallResponses =
        | SimpleValue of string 
        | Block of EthBlock 
        | TransactionHash of EthTransactionHash 
        | TransactionReceiptResult of TransactionReceipt 
        | Transaction of MinedTransaction 
        | CallResult of EVMDatatype list 
        | Library of string
        | Empty
        
        
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Ethereum MailboxProcessor types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    ///
    /// Provides signals for the logger. 'Log' will print a message to the
    /// console. 'Emit' will produce a record or other output ready for
    /// unwrapping. 'LogAndEmit' does both. 'Quiet' does neither.
    ///  
    type LogSignal =
        | Log
        | Emit
        | LogAndEmit
        | Quiet 
    
    
    ///
    /// Convenience type
    type Web3Connection = HttpRPCMessage -> Result<RPCResponse.Root, Web3Error>

    
    ///
    /// MailboxProcessor for sending and receiving the results of an RPC call.
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
    //// Web3Environment
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /// 
    /// Web3Environment is a convenience grouping of necessary functions and
    /// data to perform operations with web3.fs.
    ///  
    type Web3Environment =
        { connection: Web3Connection
          monitor: Monitor
          constants: ContractConstants
          log : LogSignal -> Result<CallResponses, Web3Error> -> CallResponses }
        
             
        
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //// Logging types
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    /// Color bindings
    type WConsoleColor =
        | Blue
        | DarkBlue
        | Yellow
        | Green
        | Red
        | Gold
    
    
    /// Signal to indicate logging behavior
    type LogType =
        | Warn
        | Success
        | Failure
    
        
    /// Simple logging message type for MailboxProcessor
    type LogMessage = LogType * CallResponses
    
    
    /// A MailboxProcessor-based logger
    type Logger = MailboxProcessor<LogMessage>
    