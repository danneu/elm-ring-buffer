module Util
    exposing
        ( applyRepeatedly
        , unwrapMaybe
        )

{-| Applies a function to a value repeatedly
piping the result of the previous application into the next.
-}


applyRepeatedly : Int -> (a -> a) -> a -> a
applyRepeatedly times fn initValue =
    List.foldl (\_ value -> fn value) initValue (List.range 0 (times - 1))


{-| Get the value out of a Maybe when you know it is always Just.
-}
unwrapMaybe : Maybe a -> a
unwrapMaybe maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash "Impossible"
