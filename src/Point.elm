module Point exposing (..)

import String


type alias Point =
    { x : Float
    , y : Float
    }


fromTuple : ( Float, Float ) -> Point
fromTuple ( x, y ) =
    Point x y


attrX : Point -> String
attrX p =
    String.fromFloat p.x


attrY : Point -> String
attrY p =
    String.fromFloat p.y
