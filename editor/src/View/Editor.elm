module View.Editor exposing (..)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Util
import View.Common


editorHeaderView : EditorModel -> Html EditorMsg
editorHeaderView model =
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
            [ span [ class "btn", onClick OnEditorGoBack ] [ text "Back" ]
            ]
            [ span [ class "docState" ] [ text state ]
            , ul []
                [ li [ class "btn render", onClick clickLayoutMsg ] [ showRenderText ]
                , li [ class "btn setting", onClick OnEditorOpenSettings ] [ text "Settings" ]
                ]
            ]
