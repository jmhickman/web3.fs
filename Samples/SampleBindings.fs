// Add binding to the end of the web3.fs namespace
namespace web3.fs.AwesomeDefiContracts

// Then just open web3.fs
open web3.fs

// Break contracts into separate modules.
module Contract1 =

    // If the contract isn't ever going to be deployed by you, just import its 
    // ABI. You can drop the whole ABI string into the source, or use a path-
    // relative import.
    let contract1ABI = returnABIFromFile "./Contracts/Contract1.abi"
    

    // Give the contract a name similar to how its commonly referred to. 
    type Contract1 =
        { chainId: string
          contract: DeployedContract
          web3Environment: Web3Environment }
        
        // Attach member functions to it as required.
        member this.contractFunction1 () = contractCall this.contract (ByName "someFunction") [] this.web3Environment

        member this.contractFunction2 () = contractCall this.contract (ByName "someOtherFunction") [] this.web3Environment

        member this.contractfunction3 argument = contractCall this.contract (ByName "finalFunction") [Address argument] this.web3Environment
        
    
    // This is the 'constructor' using 'loadDeployedContract' to place all the
    // required details into a record we can bind and interact with.
    let loadContract1 address chainId env =
        loadDeployedContract contract1ABI chainId address
        |> optimisticallyBindDeployedContract
        |> fun _contract ->
            { chainId = chainId
              contract = _contract
              web3Environment = env }

    
// A more involved example with deployment options
module Contract2 =

    // This time, we use the JSON from solc to get both ABI and bytecode at the 
    // same time.
    let contract2ABI, contract2Bytecode = returnABIAndBytecodeFromSolcJsonFile "./Contracts/contract2.json"
     
    type Contract2 =
        { chainId: string
          contract: DeployedContract
          web3Environment: Web3Environment }
        
        member this.contractFunction1 () =
            contractCall this.contract (ByName "function1") [] this.web3Environment


        member this.contractFunction2 tokenA tokenB =
            contractTransaction this.contract (ByName "function2") [Address tokenA; Address tokenB] ZEROVALUE this.web3Environment


        member this.contractFunction3 destinationAddress  = 
            contractTransaction this.contract (ByName "function3") [Address destinationAddress] ZEROVALUE this.web3Environment


        member this.contractFunction4 privilegedAddress =
            contractTransaction this.contract (ByName "function4") [Address privilegedAddress] ZEROVALUE this.web3Environment

        
        member this.contractFunction5 () = contractCall this.contract (ByName "function5") [] this.web3Environment

        
        member this.contractFunction6 () = contractCall this.contract (ByName "function6") [] this.web3Environment

        
        member this.contractFunction7 () = contractCall this.contract (ByName "function7") [] this.web3Environment

        
        member this.contractFunction8 index =
            contractCall this.contract (ByName "function8") [Uint256 index] this.web3Environment
            
                
        member this.getPairByTokenAddresses tokenA tokenB =
            contractCall this.contract (ByName "function9") [Address tokenA; Address tokenB] this.web3Environment
    
    
    // Like before, we can use this constructor to 'attach' to a deployed 
    // instance.
    let loadContract2 address chainId env =
        loadDeployedContract contract2ABI chainId address
        |> optimisticallyBindDeployedContract
        |> fun _contract ->
            { chainId = chainId
              contract = _contract
              web3Environment = env }
            
    
    // Since we have bytecode, we can also deploy an instance and automatically
    // load it
    let public deployAndLoadContract2 address1 address2 chainId env = 
        prepareDeployAndLoadContract contract2Bytecode contract2ABI chainId [Address address1; Address address2] ZEROVALUE env
        |> optimisticallyBindDeployedContract
        |> fun _contract -> 
            { chainId = chainId
              contract = _contract
              web3Environment = env }
    


