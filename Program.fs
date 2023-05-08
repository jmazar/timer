let rec readInput (agent: MailboxProcessor<string>) =
    let input = System.Console.ReadLine()
    agent.Post input
    readInput agent

let printer (inbox: MailboxProcessor<string>) =
    let timer = System.Diagnostics.Stopwatch()

    let rec messageLoop (timer: System.Diagnostics.Stopwatch) =
        async {
            let! msg = inbox.TryReceive(1000)

            match msg with
            | Some _ -> timer.Start()
            | None -> printfn "%s" (timer.Elapsed.ToString())

            return! messageLoop timer
        }

    messageLoop timer

[<EntryPoint>]
let main args =
    printfn "Please select things"

    let printerAgent = MailboxProcessor.Start(printer)

    readInput printerAgent
    0
