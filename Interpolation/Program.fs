open Interpolation.CLIArguments
open System
open Interpolation.Core


let readPointsFromStdin () =
    Seq.initInfinite (fun _ -> Console.In.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null) // Stop at EOF (null)
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.collect (fun line ->
        try
            let parts =
                line.Split([| ";"; " " |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.filter (not << String.IsNullOrWhiteSpace)

            if parts.Length >= 2 then
                match Double.TryParse(parts.[0]), Double.TryParse(parts.[1]) with
                | (true, x), (true, y) -> Seq.singleton (x, y)
                | _ ->
                    printfn "Line skipped: could not parse numbers in '%s'" line
                    Seq.empty
            else
                printfn "Line skipped: could not parse numbers in '%s'" line
                Seq.empty
        with
        | :? FormatException ->
            eprintfn "Parse error: invalid float format in line '%s'" line
            Seq.empty
        | :? OverflowException ->
            eprintfn "Parse error: number too l arge or small in line '%s'" line
            Seq.empty
        | ex ->
            eprintfn "Unexpected error: %s" ex.Message
            Seq.empty)


[<EntryPoint>]
let main argv =
    let config = parseArgs argv
    let points = readPointsFromStdin ()

    seq {
        if config.UseLinear then
            yield!
                points
                |> linearInterpolator.interpolate config.StepSize
                |> Seq.map (fun (x, y) -> ("linear", x, y))

        if config.NewtonPoints > 0 then
            yield!
                points
                |> newtonInterpolator.interpolate config.NewtonPoints config.StepSize
                |> Seq.map (fun (x, y) -> ("newton", x, y))
    }
    |> Seq.iter (fun (algo, x, y) -> printfn "%s: %.2f %.2f" algo x y)

    0
