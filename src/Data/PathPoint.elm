module Data.PathPoint exposing (PathPoint, decoder)

import Json.Decode exposing (Decoder, field, float, map3)


type alias PathPoint =
    { lat : Float
    , long : Float
    , time : Float
    }


decoder : Decoder PathPoint
decoder =
    map3 PathPoint
        (field "lat" float)
        (field "long" float)
        (field "time" float)
