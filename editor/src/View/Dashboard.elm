module View.Dashboard exposing (view)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (Html, program, div, text, a, li, span)
import Html.Attributes exposing (href, class, name, id)
import Util


{-| Entry point of the dashboard page
-}
view : Model -> Html Msg
view model =
    div [ id "dashboard-main" ]
        [ div [ id "dashboard-header" ] [ dashboardHeader model.route ]
        , div [ id "dashboard-list" ]
            [ page model ]
        ]


{-| Show breadcrumb
-}
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
                    elms =
                        "root" :: Util.splitUrl url
                in
                    div [] (headLinks elms "/dashboard")

            _ ->
                text ("failed to get route " ++ toString route)


{-| Decide if a given name is supported (editable) file type
-}
isSupportedSource : String -> Bool
isSupportedSource path =
    let
        suffixList =
            [ ".pm", ".html", ".p", "rkt" ]
    in
        List.any (\s -> String.endsWith s path) suffixList


{-| Show individual file items
-}
itemView : String -> FolderItem -> Html msg
itemView parent item =
    let
        row =
            case item of
                Directory name ->
                    a [ href (Util.concatUrl [ "/dashboard", parent, name ]) ]
                        [ text name ]

                File name ->
                    if isSupportedSource name then
                        a [ href (Util.concatUrl [ "/editor", parent, name ]) ]
                            [ text name ]
                    else
                        text name
    in
        div [ class "dashboard-list-item" ]
            [ row ]


{-| Sort all file items by type and then name. Directory comes before File.
-}
sortItems : List FolderItem -> List FolderItem
sortItems items =
    let
        toComparable item =
            case item of
                Directory name ->
                    "0" ++ name

                File name ->
                    "1" ++ name
    in
        List.sortBy toComparable items


{-| Show all file items
-}
page : Model -> Html Msg
page model =
    let
        path =
            case model.route of
                DashboardRoute p ->
                    p

                _ ->
                    let
                        _ =
                            Debug.log "Invalid route state" model.route
                    in
                        "invalid"
    in
        case model.pollenQueryResponse of
            RemoteData.NotAsked ->
                text "Not asked"

            RemoteData.Loading ->
                text "Loading"

            RemoteData.Success (FsGet get) ->
                case get of
                    FolderContents items ->
                        let
                            sortedItems =
                                sortItems items
                        in
                            div []
                                (List.map (\item -> itemView path item) sortedItems)

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
