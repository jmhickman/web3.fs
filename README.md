# web3.fs
Functional library for interacting with Web3 facilities

### Motivation

Dissatisfaction with existing solutions for interacting with Ethereum and other EVM chains. Hope to add support for Substrate-based chains later.

### Progress

Basic functionality cobbled in. Not ready for use. It will probably explode, or be silly. 

### Deps and Reqs

Removed Nethereum deps (for now). 
FSharp.Data
FSharp.Json
Writing in .Net 6

### Code of Conduct

I'm a dictator and I rule with an iron fist. I explicitly reject CoCs.

### Example Use

```
open web3.fs.Types
open web3.fs.Helpers
open web3.fs.RPCConnector

let endpointUrl = "http://127.0.0.1:1248"

// HttpRPCMessage -> Result<FSharp.Data.JsonProvider<...>.Root, string>
let web3 = createWeb3Connection endpointUrl "2.0"

// working eth call to simple storage contract on rinkeby
// UnvalidatedEthParam1559Call
let unverifiedrecordOne =
    { utxnType = "0x02"
      unonce = ""
      utoAddr = "0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276"
      ufrom = "0x2268B96E204379Ee8366505C344EBE5Cc34d3a46"
      ugas = ""
      uvalue = ""
      udata = "0x2e64cec1"
      umaxFeePerGas = ""
      umaxPriorityFeePerGas = ""
      uaccessList = []
      uchainId = "0x4" }

// meaningless data value, will revert
let unverifiedrecordTwo =
    { utxnType = "0x02"
      unonce = ""
      utoAddr = "0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276"
      ufrom = "0x2268B96E204379Ee8366505C344EBE5Cc34d3a46"
      ugas = ""
      uvalue = ""
      udata = "0x00"
      umaxFeePerGas = ""
      umaxPriorityFeePerGas = ""
      uaccessList = []
      uchainId = "0x4" }

// incorrectly formatted data value, will reject before sending to RPC
let unverifiedrecordThree =
    { utxnType = "0x02"
      unonce = ""
      utoAddr = "0x94C4E1832fdF7156AC98d7642236aDb1FBcaF276"
      ufrom = "0x2268B96E204379Ee8366505C344EBE5Cc34d3a46"
      ugas = ""
      uvalue = ""
      udata = "0x000"
      umaxFeePerGas = ""
      umaxPriorityFeePerGas = ""
      uaccessList = []
      uchainId = "0x4" }

printfn "Normal call:"

// (HttpRPCMessage -> Result<FSharp.Data.JsonProvider<...>.Root, string>) -> EthMethod -> UnvalidatedEthParam1559Call -> Result<FSharp.Data.JsonProvider<...>.Root, string>
makeEthCall web3 EthMethod.Call unverifiedrecordOne
|> printfn "%A\n"

// Partially apply
let callProviderOne = web3 |> makeEthCall

printfn "convert the output:"

callProviderOne EthMethod.Call unverifiedrecordOne
|> fun s ->
    match s with
    | Ok o ->
        o.Result
        |> strip0xAndConvertToBigInt
        |> printfn "bigint representation %A\n"
    | Error e -> printfn $"{e}\n"

printfn "Meaningless data value:"

callProviderOne EthMethod.Call unverifiedrecordTwo
|> fun s ->
    match s with
    | Ok o ->
        o.Result
        |> strip0xAndConvertToBigInt
        |> printfn "%A\n"
    | Error e -> printfn $"{e}\n"

printfn "improperly formatted data value:"

callProviderOne EthMethod.Call unverifiedrecordThree
|> fun s ->
    match s with
    | Ok o ->
        o.Result
        |> strip0xAndConvertToBigInt
        |> printfn "%A"
    | Error e -> printfn $"{e}"


```
#### Results of the above code

```
dotnet run .\Program.fs
Normal call:
Ok
  {
  "id": 1,
  "jsonrpc": "2.0",
  "result": "0x00000000000000000000000000000000000000000000000000000000000fffff"
}

convert the output:
bigint representation 1048575

Meaningless data value:
RPC error message: {
  "message": "execution reverted",
  "code": -32000
}

improperly formatted data value:
Call/TXN object 'data' value is missing or not valid
```