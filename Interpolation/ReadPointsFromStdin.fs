module Interpolation.Read

open System

let readPointsFromStdin () =
    let rec loop () =
        seq {
            match Console.In.ReadLine() with
            | null -> ()
            | line ->
                if not (String.IsNullOrWhiteSpace line) then
                    let parts =
                        line.Split([| ' '; '\t'; ',' |], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter (fun s -> not (String.IsNullOrWhiteSpace s))

                    if parts.Length >= 2 then
                        match Double.TryParse(parts.[0]), Double.TryParse(parts.[1]) with
                        | (true, x), (true, y) -> yield x, y
                        | _ -> ()

                yield! loop ()
        }

    loop ()
