module Internal exposing (Internal)

import Array.Hamt as Array exposing (Array)


type alias Internal a =
    { array : Array (Maybe a)
    , start : Int
    , end : Int
    , length : Int
    }
