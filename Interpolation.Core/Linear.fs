namespace Interpolation.Core

module linearInterpolator =
    type private State =
        { LastXOnGrid: float
          PrevPoint: float * float }

    let interpolate (step: float) =
        if step <= 0.0 then
            invalidArg "step" "Step size must be positive"

        let folder (state, outputs) newPoint =
            match state with
            | None ->
                let (startX, _) = newPoint

                (Some
                    { LastXOnGrid = startX
                      PrevPoint = newPoint },
                 Seq.empty)
            | Some st ->
                let (prevX, prevY) = st.PrevPoint
                let (lastXOnGird) = st.LastXOnGrid
                let (newX, newY) = newPoint

                let stepsBetweenNewPoint = floor ((newX - prevX) / step)
                let newLastXOnGrid = lastXOnGird + stepsBetweenNewPoint * step

                let interpolated =
                    let dx = newX - prevX

                    if dx <= 0 then
                        invalidArg "points" "x points should be increasing sequence"

                    let dy = (newY - prevY)
                    let ratio = dy / dx

                    let linearGenerator =
                        (fun currentX ->
                            if (currentX < newX) then
                                let currentY = prevY + (currentX - prevX) * ratio
                                let nextX = currentX + step
                                Some((currentX, currentY), nextX)
                            else
                                None)

                    Seq.unfold linearGenerator (lastXOnGird + step)

                (Some
                    { LastXOnGrid = newLastXOnGrid
                      PrevPoint = newPoint },
                 interpolated)

        Seq.scan folder (None, Seq.empty) >> Seq.collect snd
