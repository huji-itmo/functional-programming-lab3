namespace Interpolation.Core

module newtonInterpolator =
    type private NewtonState =
        { PrevXList: float list
          PolynomialCoefs: float list }

    type private State =
        { LastXOnGrid: float
          newton: NewtonState }


    let addPoint (state: NewtonState) (x_new: float, y_new: float) : NewtonState =
        let xs = List.rev state.PrevXList // [x0; x1; ...; x_{n-1}]
        let coeffs = List.rev state.PolynomialCoefs // [c0; c1; ...; c_{n-1}]
        let xn = x_new

        // Compute new coefficient f[x0, ..., xn]
        let newCoeff =
            List.foldBack (fun (x_i, c_i) acc -> (acc - c_i) / (xn - x_i)) (List.zip xs coeffs) y_new

        { PrevXList = x_new :: state.PrevXList
          PolynomialCoefs = newCoeff :: state.PolynomialCoefs }

    let interpolate (step: float) =
        if step <= 0.0 then
            invalidArg "step" "Step size must be positive"

        let folder (state: State option, outputs) newPoint =
            match state with
            | None ->
                let (startX, startY) = newPoint

                (Some
                    { LastXOnGrid = startX
                      newton =
                        { PrevXList = [ startX ]
                          PolynomialCoefs = [ startY ] } },

                 Seq.empty)
            | Some st ->
                let (newX, newY) = newPoint

                let prevX = List.head st.newton.PrevXList

                let stepsBetweenNewPoint = floor ((newX - prevX) / step)
                let newLastXOnGrid = st.LastXOnGrid + stepsBetweenNewPoint * step

                let newNewtonState = addPoint st.newton newPoint

                let interpolated =
                    let dx = newX - prevX

                    if dx <= 0 then
                        invalidArg "points" "x points should be increasing sequence"


                    let linearGenerator =
                        (fun currentX ->
                            if (currentX < newX) then
                                // let currentY = prevY + (currentX - prevX) * ratio
                                // let nextX = currentX + step
                                Some((currentX, currentY), nextX)
                            else
                                None)

                    Seq.unfold linearGenerator (st.LastXOnGrid + step)

                (Some
                    { LastXOnGrid = newLastXOnGrid
                      newton = newNewtonState },
                 interpolated)

        Seq.scan folder (None, Seq.empty) >> Seq.collect snd
