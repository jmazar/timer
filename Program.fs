open System.Collections.Generic

type MyTimer =
    { Name: string
      Timer: System.Diagnostics.Stopwatch }

let rec readInput (agent: MailboxProcessor<string>) =
    let input = System.Console.ReadLine()
    agent.Post input
    readInput agent


let printOutput (timer: MyTimer option) =
    match timer with
    | Some timer ->
        System.Console.Clear()
        printfn "%s: %s" timer.Name (timer.Timer.Elapsed.ToString())
    | None -> ()

let printer (inbox: MailboxProcessor<string>) =
    let timers = Dictionary<string, MyTimer>()

    let rec messageLoop (timers: Dictionary<string, MyTimer>) (currentTimer: MyTimer option) =
        async {
            let! msg = inbox.TryReceive(1000)

            let currentTimer =
                match msg with
                | Some string ->
                    match currentTimer with
                    | Some timer -> timer.Timer.Stop()
                    | None -> ()

                    let currentTimer =
                        match timers.ContainsKey(string) with
                        | true -> timers[string]
                        | false ->
                            let newtimer =
                                { Name = string
                                  Timer = System.Diagnostics.Stopwatch() }

                            timers.Add(string, newtimer)
                            newtimer

                    currentTimer.Timer.Start()
                    Some currentTimer
                | None ->
                    printOutput currentTimer
                    currentTimer

            return! messageLoop timers currentTimer
        }

    messageLoop timers None

[<EntryPoint>]
let main args =
    printfn "Please select things"

    let printerAgent = MailboxProcessor.Start(printer)

    readInput printerAgent
    0
