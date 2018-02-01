module View exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Util
import View.Dashboard
import View.Settings
import View.Common
import View.Editor
import View.Render


dashboardView : DashboardModel -> Html DashboardMsg
dashboardView model =
    case model.route of
        DashboardRoute path ->
            let
                title =
                    View.Common.breadcrumb path

                left =
                    [ span [ class "btn", onClick OnDashboardGoBack ]
                        [ text "Back" ]
                    ]

                right =
                    [ span [ class "btn", onClick OnDashboardOpenSettings ]
                        [ text "Settings" ]
                    ]

                header =
                    View.Common.makeHeader title left right
            in
                div [ class "container" ]
                    [ header
                    , View.Dashboard.view path model
                    ]

        SettingsRoute ->
            let
                left =
                    [ span [ class "btn", onClick OnDashboardGoBack ]
                        [ text "Back" ]
                    ]

                right =
                    []

                header =
                    View.Common.makeHeader (text "Settings") left right
            in
                div [ class "container" ]
                    [ header
                    , div [ class "containerBody" ]
                        [ View.Settings.view model.settings ]
                    ]

        RenderRoute sourcePath ->
            View.Render.view model.watchResponse model.renderLocation

        _ ->
            let
                left =
                    []

                right =
                    []

                header =
                    View.Common.makeHeader (text "Dashboard") left right
            in
                div [ class "container" ]
                    [ header
                    , text "unknown"
                    ]


editorView : EditorModel -> Html EditorMsg
editorView model =
    View.Editor.editorHeaderView model
