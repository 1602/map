module Request.Location exposing (list, path)

import HttpBuilder exposing (post, get, withJsonBody, withExpect, toRequest)
import Http exposing (Request, Error)
import Data.PathPoint as PathPoint exposing (PathPoint)
import Data.VisitedLocation as VisitedLocation exposing (VisitedLocation)
import Json.Decode as Decode exposing (Decoder)


apiServer : String
apiServer =
    "http://localhost:3001"



--"https://gentle-street.glitch.me"


path : Maybe Float -> Request (List PathPoint)
path time =
    apiServer
        ++ "/path"
        |> (\url ->
                case time of
                    Just t ->
                        url ++ "?time=" ++ (toString t)

                    Nothing ->
                        url
           )
        |> get
        |> withExpect (Decode.list PathPoint.decoder |> Http.expectJson)
        |> toRequest


list : Float -> Maybe ( Float, Float ) -> Request ( List VisitedLocation, PathPoint )
list zoomLevel center =
    apiServer
        ++ "/squares"
        |> (\url ->
                case center of
                    Just ( lat, long ) ->
                        url ++ "?lat=" ++ (toString lat) ++ "&long=" ++ (toString long) ++ "&zoom=" ++ (zoomLevel |> toString)

                    Nothing ->
                        url ++ "?zoom=" ++ (zoomLevel |> toString)
           )
        |> get
        |> withExpect
            (Decode.map2 (,)
                (Decode.field "squares" (Decode.list VisitedLocation.decoder))
                (Decode.field "lastLocation" PathPoint.decoder)
                |> Http.expectJson
            )
        |> toRequest
