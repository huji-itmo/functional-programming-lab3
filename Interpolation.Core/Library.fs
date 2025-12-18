namespace Interpolation.Core

module linearInterpolator =
    type private State =
        { FirstPoint: float * float
          PrevPoint: float * float }

    let interpolate (step: float) =
        if step <= 0.0 then
            invalidArg "step" "Step size must be positive"

        let folder (state, outputs) newPoint =
            match state with
            | None ->
                (Some
                    { FirstPoint = newPoint
                      PrevPoint = newPoint },
                 Seq.empty)
            | Some st ->
                let (prevX, prevY) = st.PrevPoint
                let (firstX, firstY) = st.FirstPoint
                let (newX, newY) = newPoint

                let stepsFromFirstPoint = floor ((prevX - firstX) / step)

                let nextXOnGrid = firstX + (stepsFromFirstPoint + 1.0) * step

                let interpolated =
                    let dx = newX - prevX

                    if dx <= 0 then
                        Seq.empty
                    else
                        let dy = (newY - prevY)
                        let ratio = dy / dx

                        // How many steps fit between prevX and newX?
                        // We generate points starting from prevX + step up to newX (excluding endpoints if already covered)
                        let steps =
                            Seq.unfold
                                (fun currentX ->
                                    if (currentX < newX) then
                                        let currentY = prevY + (currentX - prevX) * ratio
                                        let nextX = currentX + step
                                        Some((currentX, currentY), nextX)
                                    else
                                        None)
                                (nextXOnGrid) // start after prevX

                        steps

                // Update state to use newPoint as the previous point
                (Some
                    { FirstPoint = st.FirstPoint
                      PrevPoint = newPoint },
                 interpolated)

        // Scan through input points, carrying state and emitting interpolated seqs
        Seq.scan folder (None, Seq.empty) >> Seq.collect snd
