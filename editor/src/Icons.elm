module Icons exposing (..)

import Html exposing (Html, i, Attribute)
import Html.Attributes exposing (class)


fa : String -> Html msg
fa name =
    i [ class ("fa fa-" ++ name) ] []
