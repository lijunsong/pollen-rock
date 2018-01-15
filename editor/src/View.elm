module View exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (Html, program, div, text, a, li, span)
import Html.Attributes exposing (href, class, name, id)
import Html.Events exposing (onClick)
import Util
import View.Dashboard
import View.Settings


view : DashboardModel -> Html DashboardMsg
view model =
    case model.route of
        DashboardRoute path ->
            let
                left =
                    [ text path ]

                right =
                    []

                header =
                    makeHeader "Dashboard" left right
            in
                div []
                    [ header
                    , View.Dashboard.view model
                    ]

        SettingsRoute ->
            let
                left =
                    [ span [ class "action", onClick OnDashboardGoBack ] [ text "Back" ] ]

                right =
                    []

                header =
                    makeHeader "Settings" left right
            in
                div []
                    [ header
                    , div [ class "containerBody" ]
                        [ View.Settings.view model.settings ]
                    ]

        _ ->
            let
                left =
                    []

                right =
                    []

                header =
                    makeHeader "Dashboard" left right
            in
                div []
                    [ header
                    , text "unknown"
                    ]


makeHeader : String -> List (Html msg) -> List (Html msg) -> Html msg
makeHeader title left right =
    div [ class "header" ]
        [ div [ class "headerTop" ]
            [ span [ class "title" ] [ text title ] ]
        , div [ class "headerBottom" ]
            [ div [ class "headerLeft" ] left
            , div [ class "headerRight" ] right
            ]
        ]
