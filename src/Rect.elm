module Rect exposing (..)

import String


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


attrX : Rect -> String
attrX r =
    String.fromFloat r.x


attrY : Rect -> String
attrY r =
    String.fromFloat r.y


attrWidth : Rect -> String
attrWidth r =
    String.fromFloat r.width


attrHeight : Rect -> String
attrHeight r =
    String.fromFloat r.height


setCenter : Float -> Float -> Rect -> Rect
setCenter cx cy rect =
    let
        w =
            rect.width

        h =
            rect.height
    in
    Rect (cx - w / 2) (cy - h / 2) w h
