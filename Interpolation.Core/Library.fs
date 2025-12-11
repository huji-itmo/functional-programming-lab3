namespace Interpolation.Core

module linearInterpolator =
    let interpolate (step: float) =
        if step <= 0.0 then
            invalidArg "step" "Step size must be positive"

        Seq.scan
            (fun (prevPoint, outputs) newPoint ->
                match prevPoint with
                | None -> (Some newPoint, Seq.empty)
                | Some(px, py) ->
                    let (x, y) = newPoint

                    let interpolated =
                        if x = px then
                            Seq.empty
                        else
                            let dx = x - px
                            let dy = y - py

                            let steps =
                                Seq.unfold
                                    (fun currentX ->
                                        if currentX <= x then
                                            let ratio = (currentX - px) / dx
                                            let currentY = py + ratio * dy
                                            Some((currentX, currentY), currentX + step)
                                        else
                                            None)
                                    px

                            steps

                    (Some newPoint, interpolated))
            (None, Seq.empty)
        >> Seq.collect snd
