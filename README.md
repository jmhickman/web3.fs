# web3.fs
Functional library for interacting with Web3 facilities

### Motivation

Dissatisfaction with existing solutions for interacting with Ethereum and other EVM chains in .Net. Hope to add support for Substrate-based chains later.

### Progress

Basic functionality cobbled in, including RPC communications (but unformatted results) and a basic representation of Solidity contracts.  
Deployed contracts can now be represented and interacted with to a certain degree. Function selector and input encoding working.  
`makeEthCall` and `makeEthTxn` now functional. Getting closer to all 'basic' functionality working.

Not ready for use. It will probably explode, or be silly. 

Docs coming.

### Deps and Reqs

Removed Nethereum deps (for now).  
FSharp.Data  
FSharp.Json  
SHA3Core  
Writing in .Net 6  

### Example Use

```
open System

open web3.fs.Types
open web3.fs.Helpers
open web3.fs.ContractFunctions
open web3.fs.RPCConnector

open SHA3Core.Enums
open SHA3Core.Keccak

// Make a hashing object
let keccakDigest = newKeccakDigest

// ABI for contract at address 0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276 (rinkeby)

let deployedABI =
    """[{"inputs":[],"name":"retrieve","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"uint256","name":"num","type":"uint256"}],"name":"store","outputs":[],"stateMutability":"nonpayable","type":"function"}]"""
    |> ABI
  
// import the deployed contract    
let deployed =
    // loadDeployedContract: digest: Keccak -> address: EthAddress -> chainId: string -> abi: ABI -> LoadContractResult
    loadDeployedContract 
        keccakDigest
        ("0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276" |> EthAddress)
        "0x04" // chainId
        deployedABI
   |> bindDeployedContract
   |> List.head
   
let web3 =
    createWeb3Connection "http://127.0.0.1:1248" "2.0"
    
let retrieveArgs = Some[] 

let storeArgs = [Uint256 "100008914000990000330000012"] |> Some

// Set some constants used during call and transaction creation
let constants =
    { address =
          "0x2268B96E204379Ee8366505C344EBE5Cc34d3a46"
          |> EthAddress
      transactionType = Some "0x02"
      maxFeePerGas = None
      maxPriorityFeePerGas = None
      data = None
      blockHeight = Some LATEST }
      
printfn "Sending initial state check..."
printfn $"Contract {deployed.address} function 'retrieve'"
//makeEthCall: rpcConnection: (HttpRPCMessage -> Result<FSharp.Data.JsonProvider<...>.Root,string>) -> constants: ContractConstants -> contract: DeployedContract -> evmFunction: FunctionIndicator -> arguments: EVMDatatype list option -> Result<FSharp.Data.JsonProvider<...>.Root,string>
let res = makeEthCall web3 constants deployed ("retrieve" |> ByString) retrieveArgs

match res with
| Ok r -> printfn $"Result: {r.Result}"
| Error e -> printfn $"Call failed: {e}"

printfn "Changing chain state..."
printfn $"Contract {deployed.address} function 'store' with argument {storeArgs}"

//makeEthTxn: rpcConnection: (HttpRPCMessage -> Result<FSharp.Data.JsonProvider<...>.Root,string>) -> constants: ContractConstants -> contract: DeployedContract -> evmFunction: FunctionIndicator -> arguments: EVMDatatype list option -> value: string -> Result<FSharp.Data.JsonProvider<...>.Root,string>
match makeEthTxn web3 constants deployed ("store" |> ByString) storeArgs "0" with
|Ok r -> printfn $" Got Txn hash: {r.Result}"
|Error e -> printfn $"Call failed: {e}"

Console.ReadLine() |> ignore

printfn "checking state again..."
printfn $"Contract {deployed.address} function 'retrieve'"
let res' = makeEthCall web3 constants deployed ("retrieve" |> ByString ) retrieveArgs 
match res' with                                                        
| Ok r -> printfn $"Result: {r.Result}"                               
| Error e -> printfn $"Call failed: {e}"
```
#### Results of the above code

```
dotnet run .\Program.fs
Sending initial state check...
Contract 0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276 function 'retrieve'
Result: 0x00000000000000000000000000000000000000000052b9b5fba1eb33bc9ee68c
Changing chain state...
Contract 0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276 function 'store' with argument Some([Uint256 "100008914000990000330000012"
])
 Got Txn hash: 0x43ae0c0b61bd07720156353bf5f7ed7ca97fdedb5c319d60fffaaa2e8adff542

checking state again...
Contract 0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276 function 'retrieve'
Result: 0x00000000000000000000000000000000000000000052b9b617654cd3e607468c

```

Obviously most of the time, once a contract is imported, you would want to partially apply nearly everything up to the function
name and arguments, and set defaults relevant to your code. So  
`makeEthCall web3 constants deployed ("retrieve" |> ByString) retrieveArgs`   
might become  
`someContract retrieve empty`  
or  
`someContract store $"{addressLookup addr}" countOfItemsMinted"`
where `addressLookup` is its own partially applied call.
### Code of Conduct

I'm a dictator and I rule with an iron fist. I explicitly reject CoCs.
