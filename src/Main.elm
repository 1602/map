module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, span)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onWithOptions, onClick)
import Geolocation exposing (Error, Location)
import Request.Location
import Data.VisitedLocation exposing (VisitedLocation)
import Json.Decode as Decode exposing (Decoder, succeed)
import Data.PathPoint exposing (PathPoint)
import Dict
import Time exposing (Time, minute)
import Task
import Http
import Svg
import Svg.Attributes exposing (x, y, rx, ry, width, height, viewBox, style, cx, cy, r, transform)


---- MODEL ----


type alias Model =
    { location : Maybe Location
    , error : Maybe Error
    , watchLocation : Bool
    , lastLocation : Maybe PathPoint
    , visitedLocations : List VisitedLocation
    , loading : Bool
    , touch : Maybe Touch
    , baseOffset : ( Float, Float )
    , verticalOffset : Float
    , horizontalOffset : Float
    , zoomLevel : Float
    , path : List PathPoint
    , time : Time
    , selectedPath : Maybe Time
    }


init : ( Model, Cmd Msg )
init =
    { location = Nothing
    , error = Nothing
    , watchLocation = False
    , lastLocation = Nothing
    , visitedLocations = []
    , loading = True
    , touch = Nothing
    , baseOffset = ( 0.0, 0.0 )
    , horizontalOffset = 0.0
    , verticalOffset = 0.0
    , zoomLevel = 12.65
    , path = []
    , time = 0
    , selectedPath = Nothing
    }
        ! [ Request.Location.list 8 Nothing
                |> Http.send VisitedLocations
          , Request.Location.path Nothing
                |> Http.send Path
          , Time.now
                |> Task.perform CurrentTime
          ]



---- UPDATE ----


type Msg
    = NoOp
    | CurrentTime Time
    | TouchStart Touch
    | TouchMove Touch
    | TouchEnd
    | ZoomIn
    | ZoomOut
    | SelectPath
    | VisitedLocations (Result Http.Error ( List VisitedLocation, PathPoint ))
    | Path (Result Http.Error (List PathPoint))


newCenter : Model -> Maybe ( Float, Float )
newCenter model =
    model.lastLocation
        |> Maybe.map
            (\loc ->
                ( loc.lat + model.verticalOffset
                , loc.long + model.horizontalOffset
                )
            )


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( a1, b1 ) ( a2, b2 ) =
    let
        da =
            a1 - a2 |> abs

        db =
            b1 - b2 |> abs
    in
        da * da + db * db |> sqrt


zoomIncrement : Float -> Float
zoomIncrement z =
    z / 10


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime t ->
            { model | time = t } ! []

        Path res ->
            res
                |> Result.withDefault []
                |> (\pp -> { model | path = pp } ! [])

        VisitedLocations res ->
            res
                |> Result.mapError (toString >> Debug.log)
                |> Result.withDefault ( [], PathPoint 0 0 0 )
                |> (\( list, pp ) -> { model | loading = False, visitedLocations = list, lastLocation = Just pp } ! [])

        {-
           Move dir ->
               case model.location of
                   Just prevLocation ->
                       newLocation (move dir prevLocation) model

                   Nothing ->
                       model ! []
        -}
        NoOp ->
            ( model, Cmd.none )

        {-
           RequestLocation ->
               { model | watchLocation = True } ! [ Geolocation.nowWith { enableHighAccuracy = False, timeout = Just 30000, maximumAge = Nothing } |> Task.attempt LocationUpdated ]
        -}
        {-
           LocationChanged l ->
               newLocation l model

           LocationUpdated res ->
               case res of
                   Ok l ->
                       newLocation l model

                   Err e ->
                       { model | error = Just e } ! []
        -}
        ZoomIn ->
            { model | zoomLevel = model.zoomLevel - (zoomIncrement model.zoomLevel) }
                ! [ Request.Location.list model.zoomLevel (newCenter model)
                        |> Http.send VisitedLocations
                  ]

        ZoomOut ->
            { model | zoomLevel = model.zoomLevel + (zoomIncrement model.zoomLevel) }
                ! [ Request.Location.list model.zoomLevel (newCenter model)
                        |> Http.send VisitedLocations
                  ]

        SelectPath ->
            let
                selectedPath =
                    Just <|
                        case model.selectedPath of
                            Nothing ->
                                model.time - 86400000

                            Just t ->
                                t - 86400000
            in
                { model
                    | selectedPath =
                        selectedPath
                }
                    ! [ Request.Location.path selectedPath
                            |> Http.send Path
                      ]

        TouchStart ts ->
            { model | touch = Just ts, baseOffset = ( model.horizontalOffset, model.verticalOffset ) } ! []

        TouchEnd ->
            { model | touch = Nothing }
                ! [ Request.Location.list model.zoomLevel (newCenter model)
                        |> Http.send VisitedLocations
                  ]

        TouchMove touch ->
            case touch of
                ZoomTouch a b ->
                    let
                        currentDistance =
                            distance a b

                        initalDistance =
                            case model.touch of
                                Just (ZoomTouch a0 b0) ->
                                    distance a0 b0

                                _ ->
                                    currentDistance
                    in
                        if currentDistance < initalDistance && initalDistance - currentDistance > 25 then
                            { model | zoomLevel = model.zoomLevel + (zoomIncrement model.zoomLevel), touch = Just touch } ! []
                        else if currentDistance > initalDistance && initalDistance - currentDistance < -25 && model.zoomLevel > 2 then
                            { model | zoomLevel = model.zoomLevel - (zoomIncrement model.zoomLevel), touch = Just touch } ! []
                        else
                            model ! []

                NavigateTouch ( x, y ) ->
                    let
                        tt =
                            model.touch |> Maybe.withDefault touch

                        ( baseHor, baseVer ) =
                            model.baseOffset

                        zoomCoefficient =
                            0.000013 * model.zoomLevel

                        ( deltaX, deltaY ) =
                            case tt of
                                NavigateTouch ( x0, y0 ) ->
                                    ( (x0 - x) * zoomCoefficient
                                    , (y0 - y) * zoomCoefficient
                                    )

                                _ ->
                                    ( 0.0, 0.0 )
                    in
                        { model
                            | touch = Just tt
                            , horizontalOffset = baseHor + deltaX
                            , verticalOffset = baseVer - deltaY
                        }
                            ! []



{-
   newLocation : Location -> Model -> ( Model, Cmd Msg )
   newLocation l model =
       { model | location = Just l, error = Nothing }
           ! [ case model.location of
                   Just prevLocation ->
                       if (approx prevLocation 0 0) /= (approx l 0 0) then
                           storeVisit l
                       else
                           Cmd.none

                   Nothing ->
                       storeVisit l
             ]
-}
{-
   storeVisit : Location -> Cmd Msg
   storeVisit loc =
       Request.Location.store loc.latitude loc.longitude
           |> Http.send (\_ -> NoOp)
-}
---- VIEW ----


bup : Float -> Float -> String
bup a step =
    let
        squareSize =
            2000
    in
        a
            * squareSize
            |> floor
            |> toFloat
            |> (+) step
            |> (\a -> a / squareSize)
            |> toString


approx : PathPoint -> Float -> Float -> String
approx loc lat long =
    bup loc.lat lat ++ "," ++ (bup loc.long long)


controls : Html Msg
controls =
    div [ class "controls" ]
        [ span [ onClick ZoomIn, class "zoom" ] [ text "+" ]
        , span [ onClick ZoomOut, class "zoom" ] [ text "-" ]
        , span [ onClick SelectPath, class "zoom" ] [ text "<" ]
        ]


view : Model -> Html Msg
view model =
    if model.loading then
        span [] [ text "Loading..." ]
    else
        div
            [ Html.Attributes.style
                [ ( "width", "100%" )
                , ( "height", "100%" )
                , ( "overflow", "hidden" )
                ]
            ]
            [ renderVistedLocations model.zoomLevel model.horizontalOffset model.verticalOffset model.lastLocation model.visitedLocations model.path
            , controls
              -- text "Location service is not available"
              -- , div [] [ model.touch |> toString |> text ]
            , case model.error of
                Just e ->
                    text (toString e)

                Nothing ->
                    text ""
              {-
                 , div []
                     [ button [ onClick RequestLocation ] [ text "Locate me" ]
                     , button [ onClick (Move Up) ] [ text "Up" ]
                     , button [ onClick (Move Down) ] [ text "Down" ]
                     , button [ onClick (Move Left) ] [ text "Left" ]
                     , button [ onClick (Move Right) ] [ text "Right" ]
                     ]
              -}
            ]



{-
   horizontalOffset =
       -0.72


   verticalOffset =
       -0.02


-}


type Touch
    = NavigateTouch ( Float, Float )
    | ZoomTouch ( Float, Float ) ( Float, Float )


touchEventDecoder : Decoder Touch
touchEventDecoder =
    Decode.oneOf
        [ Decode.map2 ZoomTouch
            (decodeScreenPoint "0")
            (decodeScreenPoint "1")
        , Decode.map NavigateTouch
            (decodeScreenPoint "0")
        ]


decodeScreenPoint : String -> Decoder ( Float, Float )
decodeScreenPoint index =
    Decode.map2 (\x y -> ( x, y ))
        (Decode.at [ index, "screenX" ] Decode.float)
        (Decode.at [ index, "screenY" ] Decode.float)


ontouch : String -> Html.Attribute Msg
ontouch eventName =
    onWithOptions ("touch" ++ eventName)
        { preventDefault = True, stopPropagation = False }
        (case eventName of
            "start" ->
                Decode.field "touches" touchEventDecoder |> Decode.map TouchStart

            "end" ->
                succeed TouchEnd

            _ ->
                Decode.field "touches" touchEventDecoder |> Decode.map TouchMove
        )


renderVistedLocations : Float -> Float -> Float -> Maybe PathPoint -> List VisitedLocation -> List PathPoint -> Html Msg
renderVistedLocations zoomLevel horizontalOffset verticalOffset loc visitedLocations path =
    case loc of
        Nothing ->
            text ""

        Just location ->
            let
                centerA =
                    location.long + horizontalOffset

                centerB =
                    location.lat * 1.61 + verticalOffset
            in
                div
                    [ ontouch "start"
                    , ontouch "move"
                    , ontouch "end"
                    , Html.Attributes.style
                        [ ( "width", "100%" )
                        , ( "position", "relative" )
                        , ( "height", "100%" )
                        ]
                    ]
                    [ Svg.svg
                        [ [ centerA + (-4.0 * zoomLevel / 2000) |> toString
                          , centerB + (-4.0 * zoomLevel / 2000) |> toString
                          , 0.005 * zoomLevel |> toString
                          , 0.005 * zoomLevel |> toString
                          ]
                            |> String.join " "
                            |> viewBox
                        , transform "scale(1, -1)"
                        , style "background: #000000; height: 100%; width: 100%; opacity: 1; position: absolute; left: 0; top: 0"
                        ]
                        (List.concat
                            [ visitedLocations
                                |> printVisitedSquares
                            , printPath path
                            , printCircle location
                            ]
                        )
                    , showMap
                        { lat = 51.5443344, long = 0.6555675, time = 0 }
                      --location
                    ]


printPath : List PathPoint -> List (Svg.Svg Msg)
printPath p =
    let
        path =
            p
                |> List.map
                    (\l ->
                        ( l.lat |> round 2000
                        , l.long |> round 2000
                        )
                    )
                |> List.foldl
                    (\coords list ->
                        case list of
                            [] ->
                                [ coords ]

                            head :: tail ->
                                if head == coords then
                                    list
                                else
                                    coords :: list
                    )
                    []
                |> List.reverse

        pathPoint movement ( lat, long ) =
            movement
                ++ (toString long)
                ++ " "
                ++ (toString lat)

        printDots =
            path
                |> List.map
                    (\( lat, long ) ->
                        Svg.circle
                            [ lat |> toString |> cy
                            , long |> toString |> cx
                            , r "0.00013"
                            , Svg.Attributes.class "path-point"
                            ]
                            []
                    )
    in
        case path of
            [] ->
                [ Svg.path [] [] ]

            head :: tail ->
                (Svg.path
                    [ Svg.Attributes.d <|
                        pathPoint "M" head
                            ++ " "
                            ++ (tail |> List.map (pathPoint "L") |> String.join " ")
                    , Svg.Attributes.fill "transparent"
                    , Svg.Attributes.strokeWidth "0.0001"
                    , Svg.Attributes.class "path-line"
                      --, Svg.Attributes.strokeOpacity "0.6"
                    ]
                    []
                )
                    :: printDots


round : Float -> Float -> Float
round squareSize a =
    a
        * squareSize
        |> floor
        |> toFloat
        |> (\a -> a / squareSize)


printVisitedSquares : List VisitedLocation -> List (Svg.Svg Msg)
printVisitedSquares locations =
    let
        squareSize =
            200

        approximate locations =
            locations
                |> List.map
                    (\l ->
                        ( l.lat |> round squareSize
                        , l.long |> round squareSize
                        )
                    )
                |> List.foldl
                    (\coords ->
                        Dict.update coords
                            (\visits ->
                                case visits of
                                    Just n ->
                                        n + 1 |> Just

                                    Nothing ->
                                        1 |> Just
                            )
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map (\( ( a, b ), c ) -> { lat = a, long = b, visits = c })
    in
        locations
            |> List.map
                (\vl ->
                    Svg.rect
                        [ (toFloat vl.a / 200) - 0.0002 |> toString |> y
                        , (toFloat vl.b / 200) - 0.0002 |> toString |> x
                        , rx "0.0001"
                        , ry "0.0001"
                        , width "0.0049"
                        , height "0.0049"
                        , style
                            ("fill:#00FFFF;fill-opacity:"
                                ++ (if List.length vl.dots < 7 then
                                        "0.1"
                                    else if List.length vl.dots < 30 then
                                        "0.2"
                                    else
                                        "0.3"
                                   )
                            )
                        ]
                        []
                )


printCircle : PathPoint -> List (Svg.Svg Msg)
printCircle location =
    [ Svg.circle
        [ location.lat |> toString |> cy
        , location.long |> toString |> cx
        , r "0.00018"
        , style "fill:white;fill-opacity:0.3;"
        ]
        []
    , Svg.circle
        [ location.lat |> toString |> cy
        , location.long |> toString |> cx
        , r "0.00007"
        , style "fill:white;fill-opacity:1;"
        ]
        []
    , Svg.circle
        [ location.lat |> toString |> cy
        , location.long |> toString |> cx
        , r "0.00005"
        , style "fill:black;fill-opacity:1;"
        ]
        []
    ]


printVisitedLocation : Float -> Float -> Float -> Svg.Svg Msg
printVisitedLocation lat long visits =
    Svg.circle
        [ lat |> toString |> cy
        , long |> toString |> cx
        , if visits > 30 then
            r "0.00013"
          else if visits > 20 then
            r "0.00012"
          else if visits > 10 then
            r "0.00011"
          else
            r "0.0001"
        , style <|
            (if visits > 100 then
                "fill:#FFFF00;"
             else if visits > 50 then
                "fill:#FF0000;"
             else
                "fill:#99FFFF;"
            )
                ++ "fill-opacity:"
                ++ (if visits > 10 then
                        "1"
                    else if visits < 3 then
                        "0.4"
                    else
                        0.1 * visits |> toString
                   )
        ]
        []



{-
   Svg.rect
       [ bup lat 0 |> y
       , bup long 0 |> x
       , width "0.0005"
       , height "0.0005"
       , style <|
           (if visits > 100 then
               "fill:#FFFF00;"
            else if visits > 50 then
               "fill:#FF0000;"
            else
               "fill:#00FF00;"
           )
               ++ "fill-opacity:"
               ++ (if visits > 10 then
                       "1"
                   else if visits < 3 then
                       "0.3"
                   else
                       0.1 * visits |> toString
                  )
       ]
       []
-}


showMap : PathPoint -> Html Msg
showMap location =
    let
        lat =
            toString location.lat

        long =
            toString location.long

        long2 =
            toString (location.long + 0.00001)

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
                    ++ "&style=feature:road.local|element:geometry.fill|visibility:on|weight:2|color:yellow"
                    -- ++ "&maptype=hybrid"
                    ++
                        "&zoom=14&size=580x580&scale=2"
                    ++ "&geodesic=true"
                    ++ "&path="
                    ++ "color:0x00000050|weight:1|fillcolor:0x00000033|geodesic:true|"
                    ++ a
                    ++ "|"
                    ++ b
                    ++ "|"
                    ++ c
                    ++ "|"
                    ++ d
                    ++ "|"
                    ++ a
                    ++ "&path=color:0x4285f4|weight:10|"
                    ++ lat
                    ++ ","
                    ++ long
                    ++ "|"
                    ++ lat
                    ++ ","
                    ++ long2
                    ++ "&key=AIzaSyCEImk9Kxih0sOYMDVu0QEZ7f5shXSWDaE"
            , Html.Attributes.width 580
            , Html.Attributes.height 580
            ]
            []


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if model.watchLocation then
    -- Geolocation.changes LocationChanged
    -- else
    Time.every minute CurrentTime



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
