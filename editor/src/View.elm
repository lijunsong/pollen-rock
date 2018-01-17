module View exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (..)
import Html.Attributes exposing (href, class, name, id)
import Html.Events exposing (onClick)
import Util
import View.Dashboard
import View.Settings
import View.Common


dashboardView : DashboardModel -> Html DashboardMsg
dashboardView model =
    case model.route of
        DashboardRoute path ->
            let
                title =
                    View.Common.breadcrumb path

                left =
                    [ span [ class "action clickable", onClick OnDashboardGoBack ]
                        [ text "Back" ]
                    ]

                right =
                    [ span [ class "clickable action", onClick OnDashboardOpenSettings ]
                        [ text "Settings" ]
                    ]

                header =
                    View.Common.makeHeader title left right
            in
                div []
                    [ header
                    , View.Dashboard.view path model
                    ]

        SettingsRoute ->
            let
                left =
                    [ span [ class "action clickable", onClick OnDashboardGoBack ]
                        [ text "Back" ]
                    ]

                right =
                    []

                header =
                    View.Common.makeHeader (text "Settings") left right
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
                    View.Common.makeHeader (text "Dashboard") left right
            in
                div []
                    [ header
                    , text "unknown"
                    ]


editorView : EditorModel -> Html EditorMsg
editorView model =
    let
        state =
            stateToText model.docState

        clickLayoutMsg =
            -- iterate empty, horizontal and vertical layout
            OnLayoutChange
                (case model.layout of
                    Nothing ->
                        Just HorizontalLayout

                    Just HorizontalLayout ->
                        Just VerticalLayout

                    Just VerticalLayout ->
                        Nothing
                )

        _ =
            Debug.log "next layout" clickLayoutMsg

        showRenderText =
            case model.layout of
                Nothing ->
                    text "Render"

                Just HorizontalLayout ->
                    text "Render[-]"

                Just VerticalLayout ->
                    text "Render[|]"
    in
        View.Common.makeHeader (View.Common.breadcrumb model.filePath)
            [ span [ class "clickable action", onClick OnEditorGoBack ] [ text "Back" ]
            ]
            [ span [ class "docState" ] [ text state ]
            , ul []
                [ li [ class "clickable action fullscreen" ] [ text "Fullscreen" ]
                , li [ class "clickable action render", onClick clickLayoutMsg ] [ showRenderText ]
                , li [ class "clickable action setting", onClick OnEditorOpenSettings ] [ text "Settings" ]
                ]
            ]
