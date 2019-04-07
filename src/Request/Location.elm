module Request.Location exposing (list, path)

import Data.PathPoint as PathPoint exposing (PathPoint)
import Data.VisitedLocation as VisitedLocation exposing (VisitedLocation)
import Http exposing (Error, Request)
import HttpBuilder exposing (get, post, toRequest, withExpect, withJsonBody)
import Json.Decode as Decode exposing (Decoder)


apiServer : String
apiServer =
    --"http://localhost:3001"
    "https://gentle-street.glitch.me"


path : Maybe Float -> Request (List PathPoint)
path time =
    apiServer
        ++ "/path"
        |> (\url ->
                case time of
                    Just t ->
                        url ++ "?time=" ++ (t |> String.fromFloat)

                    Nothing ->
                        url
           )
        |> get
        |> withExpect (Decode.list PathPoint.decoder |> Http.expectJson)
        |> toRequest


list : Float -> Maybe ( Float, Float ) -> Request ( List VisitedLocation, ( PathPoint, Float, Float ) )
list zoomLevel center =
    apiServer
        ++ "/squares"
        |> (\url ->
                case center of
                    Just ( lat, long ) ->
                        url ++ "?lat=" ++ String.fromFloat lat ++ "&long=" ++ String.fromFloat long ++ "&zoom=" ++ (zoomLevel |> String.fromFloat)

                    Nothing ->
                        url ++ "?zoom=" ++ (zoomLevel |> String.fromFloat)
           )
        |> get
        |> withExpect
            (Decode.map4 (\a b c d -> ( a, ( b, c, d ) ))
                (Decode.field "squares" (Decode.list VisitedLocation.decoder))
                (Decode.field "lastLocation" PathPoint.decoder)
                (Decode.field "squareSize" Decode.float)
                (Decode.field "ratio" Decode.float)
                |> Http.expectJson
            )
        |> toRequest
