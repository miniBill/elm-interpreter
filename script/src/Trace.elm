module Trace exposing (run)

import BackendTask exposing (BackendTask)
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Eval
import Eval.Log as Log
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Rope


run : Script
run =
    Script.withCliOptions programConfig
        (\{ input } ->
            let
                ( _, _, log ) =
                    Eval.trace input
            in
            log
                |> Rope.toList
                |> List.map logLine
                |> BackendTask.combine
                |> BackendTask.map (\_ -> ())
        )


logLine : Log.Line -> BackendTask FatalError ()
logLine line =
    Script.log line.message


programConfig : Program.Config { input : String }
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build (\input -> { input = input })
                |> OptionsParser.with (Option.requiredKeywordArg "input")
            )



-- Array.toList (Array.initialize 1056 identity)
