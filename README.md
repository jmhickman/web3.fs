# web3.fs
Functional library for interacting with Web3 facilities

### Motivation

Dissatisfaction with existing solutions for interacting with Ethereum and other EVM chains in .Net. Hope to add support for Substrate-based chains later.

### Progress

Release 0.2.0 is available. Please see the release notes in the releases section about important caveats. This release has seen real use in various on-chain
endeavors and works well at this point. Still has some rough edges and the user needs to pay attention to hex values coming back from the EVM/node and how to handle them.

Better docs coming for 0.3.0.

### Deps and Reqs

FSharp.Data  
FSharp.Json  
SHA3Core  
FsHttp  

Writen in .Net 6  

### Example Use

```fsharp

#i """nuget: C:\Users\jon_h\source\"""
#r "nuget: web3.fs, 0.2.0" 

open web3.fs

///////////////////////////////////////////////////////////////////////////////////////////////////
//// Web3 Environment Setup
///////////////////////////////////////////////////////////////////////////////////////////////////

let env = createWeb3Environment "http://127.0.0.1:1248" "2.0" "0x2268b96e204379ee8366505c344ebe5cc34d3a46"

///////////////////////////////////////////////////////////////////////////////////////////////////
//// Deploy Unit Testing Contract
///////////////////////////////////////////////////////////////////////////////////////////////////

//let bytecode = returnBytecodeFromFile """C:\Users\jon_h\source\UnitTestingContract.json"""
let abi = 
  """[{"inputs":[{"internalType":"address","name":"_owner","type":"address"}],[...],"type":"receive"}]""" 
  |> ABI

let constructor = Some [Address "0x2268b96e204379ee8366505c344ebe5cc34d3a46"]
prepareUndeployedContract env bytecode constructor RINKEBY abi
|> Result.bind (deployEthContract env ZEROV )
|> env.log Log

///////////////////////////////////////////////////////////////////////////////////////////////////
//// Deployed Contract Address: 0x231757B27a7Ef50dAfD88ee44DA3B01aD23f7E4a                     ////
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
//// Partial applications and other setup
///////////////////////////////////////////////////////////////////////////////////////////////////

// Compose two functions for handling hex strings for math later
let stripAndHex =  strip0x >> hextoBigInt

// load the deployed contract
let unitTester = 
  loadDeployedContract env "0x231757B27a7Ef50dAfD88ee44DA3B01aD23f7E4a" RINKEBY abi 
  |> bindDeployedContract 
  |> List.head

// Show all functions in the contract
unitTester.functions |> List.iter(fun p -> printfn $"{p.name} {p.hash} {p.canonicalInputs}")

let callUnitTester = makeEthCall env unitTester
let transactUnitTester = makeEthTxn env unitTester
let getOwner () = callUnitTester (ByString "owner") []
let sendValue () = transactUnitTester Receive [] "4444444444"
let withdraw () = transactUnitTester (ByString "withdrawBalance") [] ZEROV

///////////////////////////////////////////////////////////////////////////////////////////////////
//// Test for Ownership
///////////////////////////////////////////////////////////////////////////////////////////////////

printfn "starting..."

// Does the owner match my wallet address?
getOwner()
|> env.log Log

///////////////////////////////////////////////////////////////////////////////////////////////////
//// Test for receive() and withdraw functionality
///////////////////////////////////////////////////////////////////////////////////////////////////

// Figure out our balance before the following transactions:
let before = 
  makeEthRPCCall env RINKEBY EthMethod.GetBalance [$"{env.constants.walletAddress}"; LATEST] 
  |> env.log Emit 
  |> unwrapSimpleValue

printfn $"**wallet balance before send: {before}**"

// Use our partial applications
let sendV = sendValue () |> env.log Emit |> unwrapTransactionReceipt
let withD = withdraw () |> env.log Emit |> unwrapTransactionReceipt

// check our balance now.
let after = 
  makeEthRPCCall env RINKEBY EthMethod.GetBalance [$"{env.constants.walletAddress}"; LATEST] 
  |> env.log Emit 
  |> unwrapSimpleValue

// Do the math, accessing the TransactionReceipt records to figure out our costs.
let sendprice = (sendV.effectiveGasPrice |> stripAndHex) * (sendV.gasUsed |> stripAndHex)
let withdrawprice = (withD.effectiveGasPrice |> stripAndHex) * (withD.gasUsed |> stripAndHex)
let total = (after |> stripAndHex) + sendprice + withdrawprice 

if total.ToString() |> bigintToHex |> prepend0x = before then printfn "MATCH" else printfn "DIDN'T MATCH"


///////////////////////////////////////////////////////////////////////////////////////////////////
//// Test basic storage functions the long way                                                 ////
///////////////////////////////////////////////////////////////////////////////////////////////////


printfn "Checking inputs"
let inputList = 
    [ (findFunction unitTester ("addr" |> Name) |> List.head, [])
      (findFunction unitTester ("setAddr" |> Name) |> List.head, [Address "0xAb8483F64d9C6d1EcF9b849Ae677dD3315835cb2"])
      (findFunction unitTester ("addr" |> Name) |> List.head, [])
      (findFunction unitTester ("setAddr" |> Name) |> List.head, [Address "0x0000000000000000000000000000000000000000"])
      (findFunction unitTester ("addr" |> Name) |> List.head, [])
      (findFunction unitTester ("aBool" |> Name) |> List.head, [])
      (findFunction unitTester ("setABool" |> Name) |> List.head, [Bool true])
      (findFunction unitTester ("aBool" |> Name) |> List.head, [])
      (findFunction unitTester ("setABool" |> Name) |> List.head, [Bool false])
      (findFunction unitTester ("aBool" |> Name) |> List.head, [])
      (findFunction unitTester ("someBytes" |> Name) |> List.head, [])
      (findFunction unitTester ("setsomeBytes" |> Name) |> List.head, [Bytes "0x4142434445464748495051525354555657585960616263646566676869"])
      (findFunction unitTester ("someBytes" |> Name) |> List.head, [])
      (findFunction unitTester ("setsomeBytes" |> Name) |> List.head, [Bytes "0x00"])
      (findFunction unitTester ("someBytes" |> Name) |> List.head, [])
      (findFunction unitTester ("someSizedBytes" |> Name) |> List.head, [])
      (findFunction unitTester ("setSomeSizedBytes" |> Name) |> List.head, [BytesSz "0x40414243444546474849505152535455"])
      (findFunction unitTester ("someSizedBytes" |> Name) |> List.head, [])
      (findFunction unitTester ("setSomeSizedBytes" |> Name) |> List.head, [BytesSz "0x00000000000000000000000000000000"])
      (findFunction unitTester ("someSizedBytes" |> Name) |> List.head, [])
      (findFunction unitTester ("tinyInt" |> Name) |> List.head, [])
      (findFunction unitTester ("setTinyInt" |> Name) |> List.head, [Int256 "-128"])
      (findFunction unitTester ("tinyInt" |> Name) |> List.head, [])
      (findFunction unitTester ("setTinyInt" |> Name) |> List.head, [Int256 "0"])
      (findFunction unitTester ("tinyInt" |> Name) |> List.head, [])
      (findFunction unitTester ("bigInt" |> Name) |> List.head, [])
      (findFunction unitTester ("setBigInt" |> Name) |> List.head, [Int256 "-545454545454545454545454"])
      (findFunction unitTester ("bigInt" |> Name) |> List.head, [])
      (findFunction unitTester ("setBigInt" |> Name) |> List.head, [Int256 "0"])
      (findFunction unitTester ("bigInt" |> Name) |> List.head, [])
      (findFunction unitTester ("tinyUint" |> Name) |> List.head, [])
      (findFunction unitTester ("setTinyUint" |> Name) |> List.head, [Uint256 "255"])
      (findFunction unitTester ("tinyUint" |> Name) |> List.head, [])
      (findFunction unitTester ("setTinyUint" |> Name) |> List.head, [Uint256 "0"])
      (findFunction unitTester ("tinyUint" |> Name) |> List.head, [])
      (findFunction unitTester ("someString" |> Name) |> List.head, [])
      (findFunction unitTester ("setSomeString" |> Name) |> List.head, [String "HaHaHa😂"])
      (findFunction unitTester ("someString" |> Name) |> List.head, [])
      (findFunction unitTester ("setSomeString" |> Name) |> List.head, [String "" ])
      (findFunction unitTester ("someString" |> Name) |> List.head, []) 
      ]
      

inputList 
|> List.map (fun (f, arg) -> 
    if arg.IsEmpty then
        callUnitTester f arg 
        |> env.log Log 
    else 
        transactUnitTester f arg ZEROV 
        |> env.log Quiet
        )
```

```powershell
starting...
[+] Call result: [Address "0x2268b96e204379ee8366505c344ebe5cc34d3a46"]
**wallet balance before send: 0x23b695e2b2e1393**
MATCH
Checking inputs
[+] Call result: [Address "0x0000000000000000000000000000000000000000"]
[+] Call result: [Address "0xab8483f64d9c6d1ecf9b849ae677dd3315835cb2"]
[+] Call result: [Address "0x0000000000000000000000000000000000000000"]
[+] Call result: [Bool false]
[+] Call result: [Bool true]
[+] Call result: [Bool false]
[+] Call result: [Bytes "0x00"]
[+] Call result: [Bytes "0x4142434445464748495051525354555657585960616263646566676869"]
[+] Call result: [Bytes "0x00"]
[+] Call result: [BytesSz "0x0"]
[+] Call result: [BytesSz "0x40414243444546474849505152535455"]
[+] Call result: [BytesSz "0x0"]
[+] Call result: [Int256 "0"]
[+] Call result: [Int256 "-128"]
[+] Call result: [Int256 "0"]
[+] Call result: [Int256 "0"]
[+] Call result: [Int256 "-545454545454545454545454"]
[+] Call result: [Int256 "0"]
[+] Call result: [Uint256 "0"]
[+] Call result: [Uint256 "255"]
[+] Call result: [Uint256 "0"]
[+] Call result: [String ""]
[+] Call result: [String "HaHaHa??"]
[+] Call result: [String ""]
```

### Code of Conduct

I'm a dictator and I rule with an iron fist. I explicitly reject CoCs.
