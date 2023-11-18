// TODO
// - Sqlite output for when I change tasks. I think this will really just be a timestamp + tag of what the new task is
// - End day, printout stats. Total timers, context switches, etc.
// - Config file?
// - Compile to binary?
open System.Collections.Generic
open System
open System.IO

type Task =
    | Work
    | Bathroom
    | Kitchen
    | Eating
    | Walk
    | Talking
    | Meeting
    | Hide


type Timers = Dictionary<Task, Diagnostics.Stopwatch>

type State =
    { timers: Timers
      currentTask: Task
      currentTimer: Diagnostics.Stopwatch
      totalTimer: Diagnostics.Stopwatch
      hideOutput: bool
      numContextSwitches: int
      filename: String }

let rec readInput (agent: MailboxProcessor<Task>) =
    let input = Console.ReadKey()

    match input.Key with
    | ConsoleKey.W -> agent.Post Work
    | ConsoleKey.B -> agent.Post Bathroom
    | ConsoleKey.K -> agent.Post Kitchen
    | ConsoleKey.E -> agent.Post Eating
    | ConsoleKey.A -> agent.Post Walk
    | ConsoleKey.T -> agent.Post Talking
    | ConsoleKey.M -> agent.Post Meeting
    | ConsoleKey.H -> agent.Post Hide
    | _ -> printfn "Unknown task shortcut"

    readInput agent


let getTaskString task =
    match task with
    | Work -> "[W]ork"
    | Bathroom -> "[B]athroom"
    | Kitchen -> "[K]itchen"
    | Eating -> "[E]ating"
    | Walk -> "W[a]lk"
    | Talking -> "[T]alking"
    | Meeting -> "[M]eeting"
    | _ -> ""

let printTask task (timer: Diagnostics.Stopwatch) =
    printfn "%-10s: %5s                  " (getTaskString task) (timer.Elapsed.ToString("hh':'mm':'ss"))

let printColoredTask task (totalTaskTimer: Diagnostics.Stopwatch) (currenTimer: Diagnostics.Stopwatch) =
    let esc = string (char 0x1B)

    printfn
        "%s[32;1m%-10s: %5s %5s%s[0m"
        esc
        (getTaskString task)
        (totalTaskTimer.Elapsed.ToString("hh':'mm':'ss"))
        (currenTimer.Elapsed.ToString("hh':'mm':'ss"))
        esc

let printOutput state =
    if state.hideOutput then
        Console.Clear()
    else
        Console.SetCursorPosition(0, 0)

        for kvp in state.timers do
            if kvp.Key = state.currentTask then
                printColoredTask kvp.Key kvp.Value state.currentTimer
            else
                printTask kvp.Key kvp.Value

        printfn "%-10s: %5s" "Total" (state.totalTimer.Elapsed.ToString("hh':'mm':'ss"))
        printfn "%-10s: %d" "Switches" state.numContextSwitches

let writeToFile filename task =
    let string =
        sprintf "%d,%s\n" (DateTimeOffset.Now.ToUnixTimeMilliseconds()) (task.ToString())

    File.AppendAllText(filename, string)

let changeTimers
    newTask
    currentTask
    (timers: Timers)
    (totalTimer: Diagnostics.Stopwatch)
    (currentTimer: Diagnostics.Stopwatch)
    filename
    =

    if currentTask <> Hide then
        timers[currentTask].Stop()
        writeToFile filename newTask

    timers[newTask].Start()
    totalTimer.Start()
    currentTimer.Restart()

let printer (inbox: MailboxProcessor<Task>) =
    let timers = Timers()
    timers.Add(Work, Diagnostics.Stopwatch())
    timers.Add(Bathroom, Diagnostics.Stopwatch())
    timers.Add(Kitchen, Diagnostics.Stopwatch())
    timers.Add(Eating, Diagnostics.Stopwatch())
    timers.Add(Walk, Diagnostics.Stopwatch())
    timers.Add(Talking, Diagnostics.Stopwatch())
    timers.Add(Meeting, Diagnostics.Stopwatch())

    let initialState =
        { timers = timers
          currentTask = Hide
          currentTimer = Diagnostics.Stopwatch()
          totalTimer = Diagnostics.Stopwatch()
          hideOutput = false
          numContextSwitches = 0
          filename = sprintf "%s.times" (DateTime.Today.ToString("yyyyMMdd")) }


    let rec messageLoop state =
        async {
            let! msg = inbox.TryReceive(100)

            match msg with
            | Some newTask when newTask = Hide -> printOutput { state with hideOutput = true }
            | Some newTask ->
                if state.currentTask <> newTask then
                    changeTimers newTask state.currentTask timers state.totalTimer state.currentTimer state.filename
            | None -> printOutput state

            let nextTask =
                match msg with
                | Some task when task <> Hide -> task
                | _ -> state.currentTask

            let newNumContextSwitches =
                if nextTask = state.currentTask then
                    state.numContextSwitches
                else
                    state.numContextSwitches + 1

            let hideOutput =
                match msg with
                | Some task when task = Hide -> not state.hideOutput
                | _ -> state.hideOutput

            return!
                messageLoop
                    { state with
                        currentTask = nextTask
                        numContextSwitches = newNumContextSwitches
                        hideOutput = hideOutput }
        }

    messageLoop initialState

[<EntryPoint>]
let main args =
    Console.Clear()
    readInput (MailboxProcessor.Start(printer))
    0
