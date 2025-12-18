module LinearInterpolationTests

open FsUnit
open Xunit
open System
open Interpolation.Core

[<Fact>]
let ``Empty input returns empty sequence`` () =
    let result = Seq.empty |> linearInterpolator.interpolate 1.0
    result |> Seq.isEmpty |> should be True

[<Fact>]
let ``Single point returns nothing`` () =

    let input = seq [ (2.0, 5.0) ]
    let result = input |> linearInterpolator.interpolate 1.0

    result |> Seq.isEmpty |> should be True


[<Fact>]
let ``Two points with exact step match`` () =
    let input = [ (0.0, 0.0); (2.0, 4.0) ]
    let expected = [ (1.0, 2.0) ]
    let result = input |> linearInterpolator.interpolate 1.0
    result |> should equal expected

[<Fact>]
let ``Non-integer step calculation`` () =
    let input = [ (0.0, 0.0); (1.0, 1.0) ]
    let expected = [ (0.3, 0.3); (0.6, 0.6); (0.9, 0.9) ]
    let result = input |> linearInterpolator.interpolate 0.3

    let tolerance = 1e-6

    Seq.zip result expected
    |> Seq.iter (fun ((x1, y1), (x2, y2)) ->
        abs (x1 - x2) < tolerance |> should be True
        abs (y1 - y2) < tolerance |> should be True)


[<Fact>]
let ``Zero step throws exception`` () =
    (fun () -> seq [ (0.0, 0.0); (1.0, 1.0) ] |> linearInterpolator.interpolate 0.0 |> ignore)
    |> should throw typeof<ArgumentException>

[<Fact>]
let ``Negative step throws exception`` () =
    (fun () -> seq [ (0.0, 0.0); (1.0, 1.0) ] |> linearInterpolator.interpolate -0.5 |> ignore)
    |> should throw typeof<ArgumentException>

[<Fact>]
let ``Two points with step larger than distance returns nothing`` () =
    let result = seq [ (0.0, 0.0); (1.0, 1.0) ] |> linearInterpolator.interpolate 10.0
    result |> Seq.isEmpty |> should be True


module NewtonInterpolationTests =

    [<Fact>]
    let ``Zero pointsCount throws exception`` () =
        (fun () -> Seq.empty |> newtonInterpolator.interpolate 0 1.0 |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    let ``Negative pointsCount throws exception`` () =
        (fun () -> Seq.empty |> newtonInterpolator.interpolate -1 1.0 |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    let ``Zero step throws exception`` () =
        (fun () -> seq [ (0.0, 0.0); (1.0, 1.0) ] |> newtonInterpolator.interpolate 2 0.0 |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    let ``Negative step throws exception`` () =
        (fun () ->
            seq [ (0.0, 0.0); (1.0, 1.0) ]
            |> newtonInterpolator.interpolate 2 -0.5
            |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    let ``Buffering phase returns no points until threshold`` () =
        let input = seq [ (0.0, 0.0); (1.0, 1.0) ]
        let result = input |> newtonInterpolator.interpolate 3 0.5
        result |> Seq.isEmpty |> should be True

    [<Fact>]
    let ``Initial generation with two points and step=1.0`` () =
        let input = seq [ (0.0, 0.0); (2.0, 4.0) ]
        let expected = [ (0.0, 0.0); (1.0, 2.0); (2.0, 4.0) ]
        let result = input |> newtonInterpolator.interpolate 2 1.0

        let tolerance = 1e-6

        Seq.zip result expected
        |> Seq.iter (fun ((x1, y1), (x2, y2)) ->
            abs (x1 - x2) < tolerance |> should be True
            abs (y1 - y2) < tolerance |> should be True)

    [<Fact>]
    let ``Subsequent point extends interpolation`` () =
        let input = seq [ (0.0, 0.0); (2.0, 4.0); (4.0, 8.0) ]

        let expected =
            [ (0.0, 0.0)
              (1.0, 2.0)
              (2.0, 4.0) // Initial generation
              (3.0, 6.0)
              (4.0, 8.0) ] // Extension from new point

        let result = input |> newtonInterpolator.interpolate 2 1.0

        let tolerance = 1e-6

        Seq.zip result expected
        |> Seq.iter (fun ((x1, y1), (x2, y2)) ->
            abs (x1 - x2) < tolerance |> should be True
            abs (y1 - y2) < tolerance |> should be True)

    [<Fact>]
    let ``Point before last generated position is skipped`` () =
        let input =
            seq
                [ (0.0, 0.0)
                  (3.0, 3.0) // Initial buffer (pointsCount=2)
                  (1.0, 100.0) ] // Point before last generated x (3.0)

        let expected = [ (0.0, 0.0); (1.0, 1.0); (2.0, 2.0); (3.0, 3.0) ]
        let result = input |> newtonInterpolator.interpolate 2 1.0

        let tolerance = 1e-6

        Seq.zip result expected
        |> Seq.iter (fun ((x1, y1), (x2, y2)) ->
            abs (x1 - x2) < tolerance |> should be True
            abs (y1 - y2) < tolerance |> should be True)

    [<Fact>]
    let ``Non-increasing x values in buffer throws exception`` () =
        let input = seq [ (1.0, 1.0); (0.0, 0.0) ]

        (fun () -> input |> newtonInterpolator.interpolate 2 0.5 |> Seq.toList |> ignore)
        |> should throw typeof<System.Exception>

    [<Fact>]
    let ``Duplicate x values throws exception`` () =
        let input = seq [ (0.0, 0.0); (0.0, 1.0) ]

        (fun () -> input |> newtonInterpolator.interpolate 2 0.5 |> Seq.toList |> ignore)
        |> should throw typeof<System.Exception>

    [<Fact>]
    let ``Quadratic interpolation with fractional step`` () =
        let input = seq [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0) ]
        let expected = [ (0.0, 0.0); (0.5, 0.25); (1.0, 1.0); (1.5, 2.25); (2.0, 4.0) ]
        let result = input |> newtonInterpolator.interpolate 3 0.5

        let tolerance = 1e-6

        Seq.zip result expected
        |> Seq.iter (fun ((x1, y1), (x2, y2)) ->
            abs (x1 - x2) < tolerance |> should be True
            abs (y1 - y2) < tolerance |> should be True)
