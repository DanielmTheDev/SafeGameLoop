module Helpers

open Fake.Core
open SpectreCoff
open Spectre.Console

let initializeContext () =
    let execContext = Context.FakeExecutionContext.Create false "build.fsx" []
    Context.setExecutionContext (Context.RuntimeContext.Fake execContext)

// Same as toConsole in SpectreCoff, except last line removed,
// due to the behaviour of the process concerning new lines.
// Will possibly be added to the package.
let toConsoleInline payload =
    payload
    |> payloadToRenderable
    |> AnsiConsole.Write

module Proc =
    module Parallel =
        let colors = [|
            Color.Aquamarine1
            Color.Yellow2
            Color.Magenta1
            Color.Cyan1
            Color.DarkBlue
            Color.DarkGoldenrod
            Color.DarkMagenta
            Color.DarkCyan
        |]

        let errorColor = Color.Red

        let print color (colored: string) (line: string) =
            Many [
                MC (color, colored)
                C (line.TrimEnd('\r', '\n'))
            ] |> toConsoleInline

        let onStdout index name (line: string) =
            match line with
            | null -> alignedRule Right $"{name} END" |> toConsole
            | s when s.Length > 0 -> print colors[index % colors.Length] $"{name}: " line
            | _ -> ()

        let onStderr name (line: string) =
            if isNull line |> not then
                print errorColor $"{name}: " line

        let redirect (index, (name, createProcess)) =
            createProcess
            |> CreateProcess.redirectOutputIfNotRedirected
            |> CreateProcess.withOutputEvents (onStdout index name) (onStderr name)

        let printStarting indexed =
            for (index, (name, c: CreateProcess<_>)) in indexed do
                let color = colors[index % colors.Length]
                let wd = c.WorkingDirectory |> Option.defaultValue ""
                let exe = c.Command.Executable
                let args = c.Command.Arguments.ToStartInfo
                MC(color, $"{name}: {wd}> {exe} {args}") |> toConsole

        let run processes =
            processes
            |> Seq.toArray
            |> Array.indexed
            |> fun indexedProcesses ->
                printStarting indexedProcesses
                indexedProcesses
            |> Array.map redirect
            |> Array.Parallel.map Proc.run

    let runSingle singleProcess =
        (0, singleProcess)
        |> Parallel.redirect
        |> Proc.run

let createProcess exe args dir =
    // Use `fromRawCommand` rather than `fromRawCommandLine`, as its behaviour is less likely to be misunderstood.
    // See https://github.com/SAFE-Stack/SAFE-template/issues/551.
    CreateProcess.fromRawCommand exe args
    |> CreateProcess.withWorkingDirectory dir
    |> CreateProcess.ensureExitCode

let dotnet args dir = createProcess "dotnet" args dir

let npm args dir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            """
                npm was not found in path. Please install it and make sure it's available from your path.
                See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info
            """
            |> failwith

    createProcess npmPath args dir

let run (name: string) singleprocess =
    alignedRule Left name |> toConsole
    (name, singleprocess)
    |> Proc.runSingle
    |> ignore

let runParallel processes =
    alignedRule Left (processes |> Seq.map fst |> String.concat ", ") |> toConsole
    processes
    |> Proc.Parallel.run
    |> ignore

let runOrDefault args =
    try
        match args with
        | [| target |] -> Target.runOrDefault target
        | _ -> Target.runOrDefault "Run"
        0
    with e ->
        e |> Dumpify.dump |> ignore
        1