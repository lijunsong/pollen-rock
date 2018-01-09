module View exposing (view)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (Html, program, div, text, a, li, span)
import Html.Attributes exposing (href, class, name, id)
import Util


{-| Entry point of the dashboard page
-}
dashboard : String -> Model -> Html Msg
dashboard path model =
    div [ id "dashboard-main" ]
        [ div [ id "dashboard-header" ] [ dashboardHeader model.route ]
        , div [ id "dashboard-list" ]
            [ list path model ]
        ]


dashboardHeader : Route -> Html Msg
dashboardHeader route =
    let
        headLinks : List String -> String -> List (Html Msg)
        headLinks elms urlPath =
            case elms of
                [] ->
                    []

                hd :: [] ->
                    let
                        newUrl =
                            Util.concatUrl [ urlPath, hd ]
                    in
                        [ a [ href urlPath ] [ text hd ] ]

                hd :: tl ->
                    let
                        newUrl =
                            Util.concatUrl [ urlPath, hd ]
                    in
                        a [ href urlPath ] [ text hd ]
                            :: span [ class "dashboard-header-sep" ] [ text "/" ]
                            :: headLinks tl newUrl
    in
        case route of
            DashboardRoute url ->
                let
                    _ =
                        Debug.log "url" url

                    elms =
                        "root" :: Util.splitUrl url
                in
                    div [] (headLinks elms "/dashboard")

            _ ->
                text ("failed to get route " ++ toString route)


itemView : String -> FolderItem -> Html msg
itemView parent item =
    let
        row =
            case item of
                Directory name ->
                    a [ href (Util.concatUrl [ parent, name ]) ]
                        [ text name ]

                File name ->
                    text name
    in
        div [ class "dashboard-list-item" ]
            [ row ]


list : String -> Model -> Html Msg
list path model =
    case model.pollenQueryResponse of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            text "Loading"

        RemoteData.Success (FsGet get) ->
            case get of
                FolderContents items ->
                    div []
                        (List.map (\item -> itemView path item) items)

                FileContents contents ->
                    div []
                        [ text contents ]

                FsError code ->
                    div []
                        [ text ("error code: " ++ (toString code)) ]

        RemoteData.Success (FsPost res) ->
            text ("receive post response: " ++ (toString res.errno))

        RemoteData.Failure error ->
            text (toString error)


view : Model -> Html Msg
view model =
    case model.route of
        DashboardRoute path ->
            let
                absPath =
                    if String.isEmpty path then
                        "/dashboard"
                    else
                        path
            in
                dashboard absPath model

        EditorRoute loc ->
            div [] [ text "editor route NYI" ]

        NotFoundRoute ->
            div [] [ text "Not supported URL" ]
