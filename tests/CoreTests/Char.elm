module CoreTests.Char exposing (suite)

import Types exposing (Value(..))
import Test exposing (Test, describe)
import TestUtils exposing (evalTest, evalTest_)


suite : Test
suite =
    describe "Char"
        [ evalTest "Char.toCode UTF-16" "Char.toCode '𝌆'" Int <| Char.toCode '𝌆'
        , evalTest "Char.fromCode UTF-16" "Char.fromCode 0x0001D306" Char <| Char.fromCode 0x0001D306
        , evalTest_ "Char.toLocaleLower 'Ì'" Char <| Char.toLocaleLower 'Ì'
        , evalTest_ "Char.toLocaleLower 'ì'" Char <| Char.toLocaleLower 'ì'
        , evalTest_ "Char.toLocaleUpper 'Ì'" Char <| Char.toLocaleUpper 'Ì'
        , evalTest_ "Char.toLocaleUpper 'ì'" Char <| Char.toLocaleUpper 'ì'
        ]
