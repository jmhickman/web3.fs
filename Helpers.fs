namespace web3.fs

[<AutoOpen>]
module Helpers =

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
        data = None
        blockHeight = Some LATEST
        defaultValue = Some "0"
        }
        
    let public createWeb3Environment url version address =
        let rpc = createWeb3Connection url version
        {connection = rpc
         monitor = createReceiptMonitor rpc
         constants = createDefaultConstants address
         digest = newKeccakDigest}
        

