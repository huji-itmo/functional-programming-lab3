namespace Interpolation.Core

module newtonInterpolator =

    // Define proper record type for Ready state
    type ReadyState =
        { Eval: float -> float
          LastGeneratedX: float
          InitialMinX: float
          InitialMaxX: float }

    // State machine for streaming Newton interpolation
    type State =
        | Buffering of buffer: (float * float) list * count: int
        | Ready of ReadyState

    let interpolate (pointsCount: int) (step: float) (points: seq<float * float>) : seq<float * float> =
        if pointsCount <= 0 then
            invalidArg "pointsCount" "Number of points must be positive"

        if step <= 0.0 then
            invalidArg "step" "Step size must be positive"

        let tolerance = 1e-10


        // Process each point in the stream
        let folder state point =
            match state with
            | Buffering(buffer, count) ->
                let newBuffer = point :: buffer
                let newCount = count + 1

                if newCount < pointsCount then
                    // Still collecting initial points
                    (Buffering(newBuffer, newCount), Seq.empty)
                else
                    // We have enough points to build polynomial
                    let pointsArray =
                        newBuffer
                        |> List.rev // Maintain input order
                        |> List.sortBy fst
                        |> List.toArray

                    // Validate strictly increasing x values
                    for i in 1 .. pointsArray.Length - 1 do
                        if fst pointsArray.[i - 1] >= fst pointsArray.[i] then
                            failwith "x values must be strictly increasing"

                    let xs = pointsArray |> Array.map fst
                    let ys = pointsArray |> Array.map snd
                    let n = xs.Length

                    // Build divided difference table
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

                    // Extract coefficients (first column of table)
                    let coef = Array.init n (fun i -> table.[i, 0])

                    // Create polynomial evaluator
                    let evalNewton x0 =
                        let mutable result = coef.[0]
                        let mutable product = 1.0

                        for i in 1 .. n - 1 do
                            product <- product * (x0 - xs.[i - 1])
                            result <- result + coef.[i] * product

                        result

                    let minX = xs.[0]
                    let maxX = xs.[n - 1]

                    // Generate initial points from min to max x
                    let initialPoints =
                        seq {
                            let mutable currentX = minX

                            while currentX <= maxX + tolerance do
                                yield (currentX, evalNewton currentX)
                                currentX <- currentX + step
                        }

                    let lastGeneratedX =
                        if maxX >= minX then
                            let lastX = floor ((maxX - minX) / step) * step + minX
                            min lastX maxX
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
                    // Generate points from last position to new point
                    let newPoints =
                        seq {
                            let mutable currentX = state.LastGeneratedX + step

                            while currentX <= x_new + tolerance do
                                let y = state.Eval currentX
                                yield (currentX, y)
                                currentX <- currentX + step
                        }

                    // Update last generated position
                    let lastX =
                        if x_new > state.LastGeneratedX then
                            let lastGenerated =
                                floor ((x_new - state.InitialMinX) / step) * step + state.InitialMinX

                            min lastGenerated x_new
                        else
                            state.LastGeneratedX

                    let newState = { state with LastGeneratedX = lastX }
                    (Ready newState, newPoints)

        // Start with buffering state
        let initialState = Buffering([], 0)

        points
        |> Seq.scan
            (fun (state, _) point ->
                let (newState, outputs) = folder state point
                (newState, outputs))
            (initialState, Seq.empty)
        |> Seq.collect snd
