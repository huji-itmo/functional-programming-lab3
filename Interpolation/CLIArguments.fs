module Interpolation.CLIArguments

open Argu

type CLIArguments =
    | [<AltCommandLine "-l">] Linear
    | [<AltCommandLine "-n">] Newton of points: int
    | [<AltCommandLine "-s">] Step of value: float
    | [<AltCommandLine "-f">] Format of format: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Linear -> "enable linear interpolation"
            | Newton _ -> "enable Newton interpolation with N points"
            | Step _ -> "set output step (default: 0.5)"
            | Format _ -> "output format (default: \"{}: {} {}\")"

type CLIResult =
    { UseLinear: bool
      NewtonPoints: int
      StepSize: float
      OutputFormat: string }

let parseArgs argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "interpolator")
    let results = parser.ParseCommandLine(argv)

    { UseLinear = results.Contains <@ Linear @>
      NewtonPoints = results.GetResult(<@ Newton @>, defaultValue = 0)
      StepSize = results.GetResult(<@ Step @>, defaultValue = 0.5)
      OutputFormat = results.GetResult(<@ Format @>, defaultValue = "{}: {} {}") }
