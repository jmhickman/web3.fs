namespace web3.fs

open web3.fs

[<AutoOpen>]
module Helpers =

    open Logger
    
    ///
    /// Convenience function that returns a ContractConstants that contains the address used for the session, along
    /// with other values ready for manipulation via the `with` statement for modifying records. If the RPC is a wallet,
    /// these defaults should work perfectly well. If the RPC is an actual Ethereum node, the gas values and transaction
    /// type should be changed as required.
    /// 
    let public createDefaultConstants (address: string) =
        {
        walletAddress = address |> EthAddress
        transactionType = None
        maxFeePerGas = None
        maxPriorityFeePerGas = None
        arguments = None
        blockHeight = Some LATEST
        defaultValue = Some "0"
        }
        

    ///
    /// Convenience function that creates all of the necessary parts of a functioning web3 environment. The record
    /// contains the rpc connection, a transaction monitor, contract constants, a Keccak digester, and the initialized
    /// logger instance.
    /// 
    let public createWeb3Environment url version address =
        let rpc = createWeb3Connection url version
        {connection = rpc
         monitor = createReceiptMonitor rpc
         constants = createDefaultConstants address
         log = startLogger() |> log }
        
    
    ///
    /// Sometimes crafting or exposing bytestrings can be used programmatically, such as to create calldata
    /// for use in low-level areas. Use with care, this is a deep internal function exposed with no guards.
    /// 
    let public returnEVMBytestring evmDatatypes = createInputByteString evmDatatypes
        

    ///
    /// For use in the common case where only one contract is being loaded, and no errors are anticipated in the
    /// parsing of the ABI.
    /// 
    let public optimisticallyBindDeployedContract (a: Result<DeployedContract, Web3Error>) = a |> bindDeployedContract |> List.head
    
    
    
    ///
    /// Combines the preparation and deployment of a contract. Automatically calls Log on environment logger. Mostly
    /// for convenience.
    /// 
    let public prepareAndDeployContract bytecode abi chainId (constructorArguments: EVMDatatype list) value env =
        prepareUndeployedContract bytecode abi chainId constructorArguments 
        |> Result.bind(fun contract -> deployEthContract contract value env )
        |> env.log Log
        |> fun _ -> ()
    
    
    ///
    /// Combines the preparation, deployment, and loading steps of contract interaction. Automatically calls Log on
    /// environment logger. Mostly for convenience.
    /// 
    let public prepareDeployAndLoadContract bytecode abi chainId (constructorArguments: EVMDatatype list) value env =
        prepareUndeployedContract bytecode abi chainId constructorArguments
        |> Result.bind(fun contract -> deployEthContract contract value env)
        |> fun tap ->
            tap
            |> env.log Log
            |> fun _ -> tap
        |> Result.bind(fun res -> 
            match res with
            | TransactionReceiptResult transactionReceipt ->
                loadDeployedContract abi chainId transactionReceipt.contractAddress.Value
            | _ -> "Result of `deployEthContract` wasn't of the expected type" |> GenericPipelineError |> Error )
                