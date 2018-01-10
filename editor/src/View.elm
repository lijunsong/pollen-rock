module View exposing (view)

import Models exposing (..)
import Html exposing (Html, program, div, text, a, li, span)
import View.Dashboard


view : Model -> Html Msg
view model =
    case model.route of
        DashboardRoute path ->
            View.Dashboard.view model

        EditorRoute loc ->
            div [] [ text "editor route NYI" ]

        NotFoundRoute ->
            div [] [ text "Not supported URL" ]
