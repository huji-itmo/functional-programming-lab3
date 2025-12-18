namespace Interpolation.Core

module newtonInterpolator =
    // Define proper record type for Ready state
    type private ReadyState =
        { Eval: float -> float
          LastGeneratedX: float
          InitialMinX: float
          InitialMaxX: float }

    // State machine for streaming Newton interpolation
    type private State =
        | Buffering of buffer: (float * float) list * count: int
        | Ready of ReadyState

    let interpolate (pointsCount: int) (step: float) (points: seq<float * float>) : seq<float * float> =
        if pointsCount <= 0 then
            invalidArg "pointsCount" "Number of points must be positive"

        if step <= 0.0 then
            invalidArg "step" "Step size must be positive"

        let tolerance = 1e-10

        let folder (state, outputs) point =
            match state with
            | Buffering(buffer, count) ->
                let newBuffer = point :: buffer
                let newCount = count + 1

                if newCount < pointsCount then
                    (Buffering(newBuffer, newCount), Seq.empty)
                else
                    // We have enough points to build polynomial
                    let pointsArray =
                        newBuffer
                        |> List.rev // Maintain input order
                        |> List.toArray

                    for i in 1 .. pointsArray.Length - 1 do
                        if fst pointsArray.[i - 1] >= fst pointsArray.[i] then
                            failwith "x values must be strictly increasing"

                    let xs = pointsArray |> Array.map fst
                    let ys = pointsArray |> Array.map snd
                    let n = xs.Length

                    let table = Array2D.zeroCreate<float> n n

                    for j in 0 .. n - 1 do
                        table.[0, j] <- ys.[j]

                    for level in 1 .. n - 1 do
                        for i in 0 .. n - level - 1 do
                            let numerator = table.[level - 1, i + 1] - table.[level - 1, i]
                            let denominator = xs.[i + level] - xs.[i]

                            if abs denominator < tolerance then
                                failwith "Duplicate x values detected"

                            table.[level, i] <- numerator / denominator

                    let coef = Array.init n (fun i -> table.[i, 0])

                    let evalNewton x0 =
                        match n with
                        | 0 -> 0.0
                        | 1 -> coef.[0]
                        | _ ->
                            let indices = seq { for i in n - 2 .. -1 .. 0 -> i }
                            Seq.fold (fun acc i -> coef.[i] + (x0 - xs.[i]) * acc) coef.[n - 1] indices

                    let minX = xs.[0]
                    let maxX = xs.[n - 1]

                    let initialPoints =
                        let generator currentX =
                            if currentX <= maxX + tolerance then
                                let y = evalNewton currentX
                                Some((currentX, y), currentX + step)
                            else
                                None

                        Seq.unfold generator minX

                    let lastGeneratedX =
                        if maxX >= minX then
                            let steps = floor ((maxX - minX) / step)
                            minX + steps * step
                        else
                            minX

                    let readyState =
                        { Eval = evalNewton
                          LastGeneratedX = lastGeneratedX
                          InitialMinX = minX
                          InitialMaxX = maxX }

                    (Ready readyState, initialPoints)

            | Ready state ->
                let (x_new, _) = point

                // Skip points before our last generated position
                if x_new <= state.LastGeneratedX + tolerance then
                    (Ready state, Seq.empty)
                else
                    // Generate points from last position to new point using unfold
                    let newPoints =
                        let generator currentX =
                            if currentX <= x_new + tolerance then
                                let y = state.Eval currentX
                                Some((currentX, y), currentX + step)
                            else
                                None

                        Seq.unfold generator (state.LastGeneratedX + step)

                    // Update last generated position
                    let lastX =
                        if x_new > state.LastGeneratedX then
                            let steps = floor ((x_new - state.InitialMinX) / step)
                            state.InitialMinX + steps * step
                        else
                            state.LastGeneratedX

                    let newState = { state with LastGeneratedX = lastX }
                    (Ready newState, newPoints)

        // Start with buffering state
        let initialState = Buffering([], 0)

        points |> Seq.scan folder (initialState, Seq.empty) |> Seq.collect snd
