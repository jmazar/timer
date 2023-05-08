// TODO
// - Print all timers, highlight active one
// - Clean up code, split some things into functions
// - Do I want to let things just be strings or should I have some fixed enums of what my options are? I feel like the latter is better, but what if I spend time on something that isn't in the list?
// - Better output for the timers
// - Sqlite output for when I change tasks. I think this will really just be a timestamp + tag of what the new task is
// - End day, printout stats. Total timers, context switches, etc.
// - Config file?
// - Compile to binary?
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
