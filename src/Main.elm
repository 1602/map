module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Geolocation exposing (Error, Location)
import Task


---- MODEL ----


type alias Model =
    { location : Maybe Location
    , error : Maybe Error
    }


init : ( Model, Cmd Msg )
init =
    { location = Nothing, error = Nothing } ! []



---- UPDATE ----


type Msg
    = NoOp
    | LocationUpdated (Result Error Location)
    | RequestLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RequestLocation ->
            model ! [ Geolocation.now |> Task.attempt LocationUpdated ]

        LocationUpdated res ->
            case res of
                Ok l ->
                    { model | location = Just l, error = Nothing } ! []

                Err e ->
                    { model | error = Just e } ! []



---- VIEW ----


bup : Float -> Int -> Float -> String
bup a step squareSize =
    a * squareSize |> floor |> (+) (step * 1000) |> toFloat |> (\a -> a / squareSize) |> toString


multiplier : Float
multiplier =
    90000


approx : Location -> Int -> Int -> String
approx loc lat long =
    bup loc.latitude lat (1.6 * multiplier) ++ "," ++ (bup loc.longitude long multiplier)


view : Model -> Html Msg
view model =
    div []
        [ case model.location of
            Just location ->
                let
                    lat =
                        toString location.latitude

                    long =
                        toString location.longitude

                    a =
                        approx location 0 0

                    b =
                        approx location 0 1

                    c =
                        approx location 1 1

                    d =
                        approx location 1 0
                in
                    img
                        [ src <|
                            "https://maps.googleapis.com/maps/api/staticmap?center="
                                ++ lat
                                ++ ","
                                ++ long
                                ++ "&zoom=14&size=640x640"
                                ++ "&geodesic=true"
                                ++ "&markers=color:black|"
                                ++ lat
                                ++ ","
                                ++ long
                                ++ "&path="
                                ++ "color:0x00000000|weight:5|fillcolor:0x0000FF53|"
                                ++ a
                                ++ "|"
                                ++ b
                                ++ "|"
                                ++ c
                                ++ "|"
                                ++ d
                                ++ ""
                        ]
                        []

            Nothing ->
                text "Location service is not available"
        , case model.error of
            Just e ->
                text (toString e)

            Nothing ->
                text ""
        , div [] [ button [ onClick RequestLocation ] [ text "Locate me" ] ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
