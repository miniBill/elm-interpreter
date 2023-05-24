module Elm.Kernel.String exposing (all, foldr, map)


all : (Char -> Bool) -> String -> Bool
all f s =
    List.all f (String.toList s)


map : (Char -> Char) -> String -> String
map f s =
    String.fromList (List.map f (String.toList s))


foldr : (Char -> b -> b) -> b -> String -> b
foldr f i s =
    case String.uncons (String.right 1 s) of
        Nothing ->
            i

        Just ( c, _ ) ->
            foldr f (f c i) (String.dropRight 1 s)
