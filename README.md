# web3.fs
A library offering a more functional interface to Ethereum/EVM RPCs.

### Progress

Release 0.3.0 is available. This is a semi-mature release covering most functionality from an RPC library. 

### Deps and Reqs

FSharp.Data  
FSharp.Json  
SHA3Core  
FsHttp  

Writen in .Net 6  

### Example Use

```fsharp

// Build local nuget package and reference
open Web3.fs

// create a web3 environment, a record which contains an RPC endpoint, transaction monitor
// and logger.
let env = createWeb3Environment "http://127.0.0.1" "2.0" "0x426F..."

// If you log frequently, make a partial application
let logIt = env.log Log

// load your contract ABI and bytecode
let abi, bytecode = returnABIAndBytecodeFromSolcJsonFile "./MyContract.json"

//Deploy and load in one step. GANACHE is a constant for the chainId, set to 0x539
// Our contract takes one constructor argument, and isn't payable.
let contract = 
    prepareDeployAndLoadContract bytecode abi GANACHE [Address "0xccfe00b..."] ZEROVALUE env
    |> optimisticallyBindDeployedContract
  
// call a function on your contract
contractCall contract (ByName "estimateYield") [] env |> logIt

// [+] Success
// Call result: 1500364000000

// make a transaction (deposit ETH)
contractTransaction contract Receieve [] ((Ether "1") |> asWei) env |> logIt

// [@] Monitoring transaction 0x03b804ffaff877f9ea492ab878c13471a3f98c95fe7de9161464f52fba752064
// [+] Success
// <transaction data>

// oops, our contract has an overloaded function. 
contractTransaction contract (ByName "withdraw") [] ZEROVALUE env |> logIt
// [!] AmbiguousFunctionError

// We want this one
contractTransaction contract (ByNameAndMutability ("withdraw", Nonpayable)) [] ZEROVALUE env |> logIt
// [@] Monitoring transaction 0x0738a45aaff817f9ebb92ab8786f5371a3f98c95fe7de9161464f52fbaee54c0
// [+] Success
// <transaction data>
```

### Code of Conduct

I'm a dictator and I rule with an iron fist. I explicitly reject CoCs.
