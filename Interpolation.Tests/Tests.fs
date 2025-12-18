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

    // Compare with tolerance for floating point
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
