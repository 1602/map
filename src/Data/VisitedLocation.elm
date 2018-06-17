module Data.VisitedLocation exposing (VisitedLocation, decoder)

import Json.Decode as Decode exposing (Decoder, field, float, int)
import Bitwise


type alias VisitedLocation =
    { a : Int, b : Int, dots : List Dot }


type alias Dot =
    { a : Int, b : Int, s : Int }


decoder : Decoder VisitedLocation
decoder =
    Decode.list int
        |> Decode.andThen
            (\list ->
                case list of
                    a :: b :: compressedRows ->
                        compressedRows
                            |> decompress
                            |> VisitedLocation a b
                            |> Decode.succeed

                    _ ->
                        Decode.fail "Unrecognised data structure"
            )


decompress : List Int -> List Dot
decompress numbers =
    numbers
        |> List.map deflate
        |> List.indexedMap
            (\b list ->
                list
                    |> List.indexedMap
                        (\a s ->
                            Dot a b s
                        )
                    |> List.filter (\d -> d.s > 0)
            )
        |> List.concat


deflate : Int -> List Int
deflate num =
    List.repeat 10 0
        |> List.foldl
            (\i ( res, n ) ->
                ( (7 |> Bitwise.and n) :: res, n |> Bitwise.shiftRightBy 3 )
            )
            ( [], num )
        |> (\( x, _ ) -> x)
