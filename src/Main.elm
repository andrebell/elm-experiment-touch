module Main exposing (..)

import Browser
import Html as H exposing (Html, div, h1, img, text)
import Html.Attributes as HA exposing (src)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Point exposing (Point)
import Rect exposing (Rect)
import String
import Svg as S
import Svg.Attributes as SA
import Time
import Tuple exposing (first, second)



---- CONSTANTS ----


dt =
    -- Time tick in ms
    1000 / 25



---- MODEL ----


type alias Model =
    { rect : Rect
    , alpha : Float
    , point : Point
    , pd : Point
    , pm : Point
    , pu : Point
    , duration : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Rect 10 10 100 100)
        0
        (Point 500 500)
        (Point 0 0)
        (Point 0 0)
        (Point 0 0)
        2000
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = MouseDownAt ( Float, Float )
    | PointerDownAt ( Float, Float )
    | PointerMoveAt ( Float, Float )
    | PointerUpAt ( Float, Float )
    | Tick Time.Posix


distanceTL : ( Float, Float ) -> Float
distanceTL ( x, y ) =
    sqrt (x ^ 2 + y ^ 2)


relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos


updateRect : Model -> Rect -> Rect
updateRect model rect =
    let
        cx =
            500 + model.point.x / 2 * cos model.alpha

        cy =
            500 + model.point.y / 2 * sin model.alpha
    in
    Rect.setCenter cx cy rect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDownAt offset ->
            let
                x =
                    screenToViewBox <| first offset

                y =
                    screenToViewBox <| second offset
            in
            ( { model
                | point =
                    Point x y
                , duration = 10 * distanceTL ( x, y )
              }
            , Cmd.none
            )

        PointerDownAt offset ->
            let
                x =
                    screenToViewBox <| first offset

                y =
                    screenToViewBox <| second offset
            in
            ( { model
                | pd = Point x y
                , point = Point x y
                , duration = 10 * distanceTL ( x, y )
              }
            , Cmd.none
            )

        PointerMoveAt offset ->
            let
                x =
                    screenToViewBox <| first offset

                y =
                    screenToViewBox <| second offset
            in
            ( { model
                | pm = Point x y
                , point = Point x y
                , duration = 10 * distanceTL ( x, y )
              }
            , Cmd.none
            )

        PointerUpAt offset ->
            let
                x =
                    screenToViewBox <| first offset

                y =
                    screenToViewBox <| second offset
            in
            ( { model
                | pu = Point x y
                , point = Point x y
                , duration = 10 * distanceTL ( x, y )
              }
            , Cmd.none
            )

        Tick time ->
            let
                d_alpha =
                    2 * pi * dt / model.duration

                newalpha =
                    model.alpha + d_alpha

                newmodel =
                    { model
                        | alpha = newalpha
                    }
            in
            ( { newmodel
                | rect = updateRect newmodel model.rect
              }
            , Cmd.none
            )



---- VIEW ----


screenToViewBox : Float -> Float
screenToViewBox value =
    1000 * value / 400


view : Model -> Html Msg
view model =
    div []
        [ S.svg
            [ SA.width "400"
            , SA.viewBox "0 0 1000 1000"
            , HA.attribute "style" "border: 1px solid black"

            --, Mouse.onDown (\event -> MouseDownAt event.offsetPos)
            , HA.style "touch-action" "none"
            ]
            [ S.ellipse
                [ SA.cx "500"
                , SA.cy "500"
                , SA.rx <| String.fromFloat <| model.point.x / 2
                , SA.ry <| String.fromFloat <| model.point.y / 2
                , SA.stroke "black"
                , SA.strokeWidth "1"
                , SA.fill "none"
                ]
                []
            , S.line
                [ SA.x1 "0"
                , SA.y1 "0"
                , SA.x2 <| Point.attrX model.point
                , SA.y2 <| Point.attrY model.point
                , SA.stroke "green"
                , SA.strokeWidth "2"
                ]
                []
            , S.rect
                [ SA.id "rect"
                , SA.x <| Rect.attrX model.rect
                , SA.y <| Rect.attrY model.rect
                , SA.width <| Rect.attrWidth model.rect
                , SA.height <| Rect.attrHeight model.rect
                , SA.rx "10"
                , SA.ry "10"
                , SA.fill "#0000ff"
                ]
                []
            , S.circle
                [ SA.cx <| Point.attrX model.point
                , SA.cy <| Point.attrY model.point
                , SA.r "20"
                , SA.stroke "black"
                , SA.strokeWidth "6"
                , SA.fill "green"
                , Pointer.onDown (relativePos >> PointerDownAt)
                , Pointer.onMove (relativePos >> PointerMoveAt)
                , Pointer.onUp (relativePos >> PointerUpAt)
                ]
                []
            ]

        --, H.p [] [ H.text <| "cx: " ++ (Debug.toString <| model.point.x) ]
        --, H.p [] [ H.text <| "cy: " ++ (Debug.toString <| model.point.y) ]
        , H.p [] [ H.text <| "p: (" ++ String.fromFloat model.point.x ++ "," ++ String.fromFloat model.point.y ++ ")" ]
        , H.p [] [ H.text <| "duration (ms): " ++ String.fromFloat model.duration ]
        , H.p [] [ H.text <| "pointer down: (" ++ String.fromFloat model.pd.x ++ "," ++ String.fromFloat model.pd.y ++ ")" ]
        , H.p [] [ H.text <| "pointer move: (" ++ String.fromFloat model.pm.x ++ "," ++ String.fromFloat model.pm.y ++ ")" ]
        , H.p [] [ H.text <| "pointer up: (" ++ String.fromFloat model.pu.x ++ "," ++ String.fromFloat model.pu.y ++ ")" ]
        ]



---- SUBSCRIPTIONS ----


subscriptions model =
    Time.every dt Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
