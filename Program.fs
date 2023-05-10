// TODO
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


type Timers = Dictionary<Task, Diagnostics.Stopwatch>

let rec readInput (agent: MailboxProcessor<Task>) =
    let input = Console.ReadKey()

    match input.Key with
    | ConsoleKey.W -> agent.Post Work
    | ConsoleKey.B -> agent.Post Bathroom
    | ConsoleKey.K -> agent.Post Kitchen
    | ConsoleKey.E -> agent.Post Eating
    | ConsoleKey.A -> agent.Post Walk
    | _ -> printfn "Unknown task shortcut"

    readInput agent


let getTaskString task =
    match task with
    | Work -> "[W]ork"
    | Bathroom -> "[B]athroom"
    | Kitchen -> "[K]itchen"
    | Eating -> "[E]ating"
    | Walk -> "W[a]lk"

let printOutput (currentTask: Task option) (timers: Timers) contextSwitches =
    Console.SetCursorPosition(0, 0)

    for kvp in timers do
        match currentTask with
        | Some task ->
            if kvp.Key = task then
                let esc = string (char 0x1B)
                printfn "%s[32;1m%-10s: %5s%s[0m" esc (getTaskString kvp.Key) (kvp.Value.Elapsed.ToString()) esc
            else
                printfn "%-10s: %5s" (getTaskString kvp.Key) (kvp.Value.Elapsed.ToString())
        | None -> printfn "%-10s: %5s" (getTaskString kvp.Key) (kvp.Value.Elapsed.ToString())

    printfn "%-10s: %d" "Switches" contextSwitches

    currentTask

let changeTimers newTask currentTask (timers: Timers) =
    match currentTask with
    | Some currentTask -> timers[currentTask].Stop()
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


    let rec messageLoop (timers: Timers) (currentTask: Task option) numContextSwitches =
        async {
            let! msg = inbox.TryReceive(100)

            let newCurrentTask =
                match msg with
                // TODO match on neWTask as well and do stuff if quit
                | Some newTask -> changeTimers newTask currentTask timers
                | None -> printOutput currentTask timers numContextSwitches

            let newNumContextSwitches =
                if newCurrentTask = currentTask then
                    numContextSwitches
                else
                    numContextSwitches + 1

            return! messageLoop timers newCurrentTask newNumContextSwitches
        }

    messageLoop timers None 0

[<EntryPoint>]
let main args =
    Console.Clear()
    readInput (MailboxProcessor.Start(printer))
    0
