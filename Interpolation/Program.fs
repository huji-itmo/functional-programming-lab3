open Interpolation.CLIArguments
open Interpolation.Read
open System
open Interpolation.Core.linearInterpolator


[<EntryPoint>]
let main argv =
    let config = parseArgs argv
    let points = readPointsFromStdin ()

    let results =
        seq {
            if config.UseLinear then
                yield!
                    points
                    |> interpolate config.StepSize
                    |> Seq.map (fun (x, y) -> ("linear", x, y))

        // if config.NewtonPoints > 0 then
        //     yield! points |> newtonInterpolation config.NewtonPoints config.StepSize
        }

        |> Seq.sortBy (fun (_, x, _) -> x)

    for (algo, x, y) in results do
        Console.WriteLine(
            config.OutputFormat.Replace("{}", algo).Replace("{}", sprintf "%.4f" x).Replace("{}", sprintf "%.4f" y)
        )


    0
