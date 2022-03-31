# web3.fs
Functional library for interacting with Web3 facilities

### Motivation

Dissatisfaction with existing solutions for interacting with Ethereum and other EVM chains in .Net. Hope to add support for Substrate-based chains later.

### Progress

Release 0.1.0 is available. Please see the release notes in the releases section about important caveats. TL;DR you still probably shouldn't use it yet.

Docs coming for 0.2.0.

### Deps and Reqs

FSharp.Data  
FSharp.Json  
SHA3Core  
FsHttp
Writing in .Net 6  

### Example Use

```fsharp
open web3.fs

// Prepare a hash digest, some constants, a web3 RPC connection, and a monitor for watching transaction receipts.
// Private keys are handled in the wallet. Use Frame.sh! https://frame.sh 
// Private keys sitting around are bad, mkay? Environment vars aren't a suitable replacement. :)

let keccakDigest = newKeccakDigest
let constants = createDefaultConstants "0x2268b96e204379ee8366505c344ebe5cc34d3a46" // the wallet that will source transactions
let web3c = createWeb3Connection "http://127.0.0.1:1248" "2.0" // Frame's listener and the RPC version
let monitor: Monitor = createReceiptMonitor web3c

// The ABI of the contract to deploy. Here as a single line of text, but could be in a file as well.
let testContractABI =
  """[{"inputs":[{"internalType":"address","name":"aa","type":"address"}]<SNIP>""" |> ABI
// Get bytecode from a file.
let testContractBytecode =
  returnBytecodeFromFile """C:\Users\jon\source\repos\web3.fs\Samples\Contract.json"""

// Deploy to Rinkeby. First the contract is parsed and loaded, then deployed. The transaction is
// monitored, and when it fails or succeeds it is logged to the console automatically. 
prepareUndeployedContract keccakDigest testContractBytecode None RINKEBY testContractABI
|> Result.bind (deployEthContract web3c constants ) 
|> monitorTransaction monitor
|> ignore

(*
Beginning monitoring of transaction "0x765bff94b73cd6eb0d676662b86d80b5f8caef1abc678f8380dacb7020259939"
Transaction receipt: { blockHash =
   "0xb8a33e7e116346b173853411eda2521f2cb11d3bbbd3ed726be6c333950b02fb"
  blockNumber = "0x9ee7da"
  contractAddress = Some "0x1a4bbe1d61b88c883a110d78105d98d683871c74"
  cumulativeGasUsed = "0xa8838a"
  effectiveGasPrice = "0x3b9aca09"
  from = "0x2268b96e204379ee8366505c344ebe5cc34d3a46"
  gasUsed = "0x119239"
  logs = []
  logsBloom =
   "0x00000000000000000000000000000000000000000000000000000000000000000000000000000<SNIP>"
  status = "0x1"
  toAddr = "null"
  transactionHash =
   "0x765bff94b73cd6eb0d676662b86d80b5f8caef1abc678f8380dacb7020259939"
  transactionIndex = "0x35"
  tType = "0x2"
  isNull = false }
*)

// After deployment, let's interact. We switch to the deployed contract
let testContractAddress = "0x1a4bbe1d61b88c883a110d78105d98d683871c74" |> EthAddress
let deployed = loadDeployedContract keccakDigest testContractAddress RINKEBY testContractABI |> bindDeployedContract |> List.head

// Let's partially apply the contract to make it easier to interact with
let callContract = makeEthCall web3c constants deployed
let contractTransaction = makeEthTxn web3c constants deployed
    
// Let's save a couple of functions as well, to reference them
let doNothing0 = findFunction (Name "doNothing0") deployed |> List.head
let byFunctionHash = ("0x035dee5e" |> EVMFunctionHash |> SearchFunctionHash)
let seeNothing0 = findFunction byFunctionHash deployed |> List.head

(*
[IndicatedFunction { name = "doNothing0"
                    hash = EVMFunctionHash "0x2a9f633a"
                    inputs = EVMFunctionInputs "(address)"
                    outputs = []
                    config = Nonpayable }]
[IndicatedFunction { name = "seeNothing0"
                    hash = EVMFunctionHash "0x035dee5e"
                    inputs = EVMFunctionInputs "()"
                    outputs = [Address ""]
                    config = Nonpayable }]
*)

// Let's use one of our partials to call the contract and read a value. An empty list is used to indicate an empty argument
// tuple. `seeNothing0` is our function from before.
callContract seeNothing0 []
|> ignore

// Call successful, got: "0x0000000000000000000000000000000000000000000000000000000000000000"

// Now let's set a value. Arguments are a `Some`, and are a list. '0' is the value argument, but doNothing is
// non-payable. We'll also monitor this transaction once we get the hash back
contractTransaction doNothing0 [Address "0x2268b96e204379ee8366505c344ebe5cc34d3a46"] "0"
|> monitorTransaction monitor
|> ignore

// Then follow up with re-reading the value. Here, you can see that you may use a Some empty list if you wish.
callContract seeNothing0 []
|> ignore

(*
Call successful, got: "0x0000000000000000000000000000000000000000000000000000000000000000"

Beginning monitoring of transaction "0xb2b50ef8208928dafedc83afef0c9fe2a6a4cf5805db64f073671ab4cb7fee45"
Transaction receipt: { blockHash =
   "0x9701b56960d5b002fba16e7b30e8fe8f37fbbbd5daa76c4ee36e70f5d8dae1e6"
  blockNumber = "0x9ee7e2"
  contractAddress = Some "null"
  cumulativeGasUsed = "0xabcd34"
  effectiveGasPrice = "0x3b9aca0a"
  from = "0x2268b96e204379ee8366505c344ebe5cc34d3a46"
  gasUsed = "0xac59"
  logs = []
  logsBloom =
   "0x000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000<SNIP>"
  status = "0x1"
  toAddr = ""0x1a4bbe1d61b88c883a110d78105d98d683871c74""
  transactionHash =
   "0xb2b50ef8208928dafedc83afef0c9fe2a6a4cf5805db64f073671ab4cb7fee45"
  transactionIndex = "0x33"
  tType = "0x2"
  isNull = false }

Call successful, got: "0x0000000000000000000000002268b96e204379ee8366505c344ebe5cc34d3a46"
*)
```

### Code of Conduct

I'm a dictator and I rule with an iron fist. I explicitly reject CoCs.
