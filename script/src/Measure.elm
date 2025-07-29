module Measure exposing (run)

import BackendTask.Custom
import BackendTask.Do as Do
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Eval
import Json.Decode
import Json.Encode
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withCliOptions programConfig
        (\{ input } ->
            Do.log "Starting measurement" <| \_ ->
            Do.allowFatal (BackendTask.Custom.run "profile" Json.Encode.null (Json.Decode.succeed ())) <| \_ ->
            let
                _ =
                    Eval.eval (Maybe.withDefault "Array.toList (Array.initialize 1056 identity)" input)
            in
            Do.allowFatal (BackendTask.Custom.run "profileEnd" Json.Encode.null (Json.Decode.succeed ())) <| \_ ->
            Do.noop
        )


programConfig : Program.Config { input : Maybe String }
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build (\input -> { input = input })
                |> OptionsParser.with (Option.optionalKeywordArg "input")
            )
