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
open System

type Task =
    | Work
    | Bathroom
    | Kitchen
    | Eating
    | Walk
    | Quit


type Timers = Dictionary<Task, Diagnostics.Stopwatch>

let rec readInput (agent: MailboxProcessor<Task>) =
    let input = Console.ReadKey()

    match input.Key with
    | ConsoleKey.W -> agent.Post Work
    | ConsoleKey.B -> agent.Post Bathroom
    | ConsoleKey.K -> agent.Post Kitchen
    | ConsoleKey.E -> agent.Post Eating
    | ConsoleKey.L -> agent.Post Walk
    | ConsoleKey.Q -> agent.Post Quit
    | _ -> printfn "Unknown task shortcut"

    readInput agent


let printOutput (currentTask: Task option) (timers: Timers) =
    Console.Clear()
    for kvp in timers do
        printfn "%s: %s" (kvp.Key.ToString()) (kvp.Value.Elapsed.ToString())

    currentTask

let changeTimers newTask currentTask (timers: Timers) =
    match currentTask with
    | Some currentTask -> 
        timers[currentTask].Stop()
    | None -> ()
    timers[newTask].Start()
    Some newTask


let printer (inbox: MailboxProcessor<Task>) =
    let timers = Timers()
    timers.Add(Work, Diagnostics.Stopwatch())
    timers.Add(Bathroom, Diagnostics.Stopwatch())
    timers.Add(Kitchen, Diagnostics.Stopwatch())
    timers.Add(Eating, Diagnostics.Stopwatch())
    timers.Add(Walk, Diagnostics.Stopwatch())


    let rec messageLoop (timers: Timers) (currentTask: Task option) =
        async {
            let! msg = inbox.TryReceive(100)

            let currentTask =
                match msg with
                // TODO match on neWTask as well and do stuff if quit
                | Some newTask -> changeTimers newTask currentTask timers
                | None -> printOutput currentTask timers

            return! messageLoop timers currentTask
        }

    messageLoop timers None

[<EntryPoint>]
let main args =
    let printerAgent = MailboxProcessor.Start(printer)

    readInput printerAgent
    0
