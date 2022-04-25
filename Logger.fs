namespace web3.fs

module Logger =
    
    open System
    
    ///
    /// Just color bindings.
    let setColor color = 
        match color with
        | Blue -> Console.ForegroundColor <- ConsoleColor.Blue
        | Yellow -> Console.ForegroundColor <- ConsoleColor.Yellow
        | Green -> Console.ForegroundColor <- ConsoleColor.Green
        | Red -> Console.ForegroundColor <- ConsoleColor.Red
        
    
    ///
    /// Revert the color.
    let revertColor () =Console.ResetColor()
        
        
    ///
    /// Emits console messages with color and glyphs based on the incoming message. I'm not certain these locks
    /// are required, but better safe than sorry I suppose.
    let private loggerMailbox (mbox: Logger) =
        let locker = Object
        let rec receiver () =
            async {
                let! msg = mbox.Receive()
                let logType, message = msg
                match logType with
                | Info ->
                    lock locker (fun _ ->
                    setColor Blue
                    printf "[*] "
                    revertColor ()
                    printfn $"{message}")
                | Warn ->
                    lock locker (fun _ ->
                    setColor Yellow
                    printf "[-] "
                    revertColor ()
                    printfn $"{message}")
                | Success ->
                    lock locker (fun _ ->
                    setColor Green
                    printf "[+] "
                    revertColor ()
                    printfn $"{message}")
                | Failure ->
                    lock locker (fun _ ->
                    setColor Red
                    printf "[!] "
                    revertColor ()
                    printfn $"{message}")
                
                do! receiver () 
            }
        receiver ()
        
    
    let internal startLogger () = MailboxProcessor.Start(loggerMailbox)