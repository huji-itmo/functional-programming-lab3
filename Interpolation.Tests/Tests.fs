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
let ``Single point returns original point`` () =
    let input = seq [ (2.0, 5.0) ]
    let expected = [ (2.0, 5.0) ]
    let result = input |> linearInterpolator.interpolate 1.0
    result |> should equal expected

[<Fact>]
let ``Two points with step larger than distance`` () =
    let input = [ (0.0, 0.0); (1.0, 1.0) ]
    let expected = [ (0.0, 0.0); (1.0, 1.0) ]
    let result = input |> linearInterpolator.interpolate 2.0
    result |> should equal expected

[<Fact>]
let ``Two points with exact step match`` () =
    let input = [ (0.0, 0.0); (2.0, 4.0) ]
    let expected = [ (0.0, 0.0); (1.0, 2.0); (2.0, 4.0) ]
    let result = input |> linearInterpolator.interpolate 1.0
    result |> should equal expected

[<Fact>]
let ``Non-integer step calculation`` () =
    let input = [ (0.0, 0.0); (1.0, 1.0) ]
    let expected = [ (0.0, 0.0); (0.3, 0.3); (0.6, 0.6); (0.9, 0.9); (1.0, 1.0) ]
    let result = input |> linearInterpolator.interpolate 0.3

    // Compare with tolerance for floating point
    let tolerance = 1e-6

    Seq.zip result expected
    |> Seq.iter (fun ((x1, y1), (x2, y2)) ->
        abs (x1 - x2) < tolerance |> should be True
        abs (y1 - y2) < tolerance |> should be True)


[<Fact>]
let ``Vertical line (zero dx)`` () =
    let input = [ (1.0, 1.0); (1.0, 3.0); (1.0, 2.0) ]
    let expected = [ (1.0, 1.0); (1.0, 3.0); (1.0, 2.0) ]
    let result = input |> linearInterpolator.interpolate 0.5
    result |> should equal expected


[<Fact>]
let ``Zero step throws exception`` () =
    (fun () -> seq [ (0.0, 0.0); (1.0, 1.0) ] |> linearInterpolator.interpolate 0.0 |> ignore)
    |> should throw typeof<ArgumentException>

[<Fact>]
let ``Negative step throws exception`` () =
    (fun () -> seq [ (0.0, 0.0); (1.0, 1.0) ] |> linearInterpolator.interpolate -0.5 |> ignore)
    |> should throw typeof<ArgumentException>

[<Fact>]
let ``Large dataset stress test`` () =
    // Generate 1000 points with random gaps
    let rand = Random(42)

    let points =
        [| 0..1000 |]
        |> Array.map (fun i -> float i + rand.NextDouble(), sin (float i) * 10.0)
        |> Seq.ofArray

    // Should complete without errors
    let result = points |> linearInterpolator.interpolate 0.75
    Seq.length result |> should be (greaterThan (Seq.length points))
