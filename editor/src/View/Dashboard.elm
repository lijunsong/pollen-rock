module View.Dashboard exposing (view)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (Html, program, div, text, a, li, span)
import Html.Attributes exposing (href, class, name, id)
import Util
import View.Common


{-| Entry point of the dashboard page
-}
view : String -> DashboardModel -> Html DashboardMsg
view pathname model =
    div [ id "dashboard-main" ]
        [ page pathname model
        ]


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
                        [ text (name ++ "/") ]

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
page : String -> DashboardModel -> Html DashboardMsg
page path model =
    case model.fsListDirectory of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            text "Loading"

        RemoteData.Success get ->
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

        RemoteData.Failure error ->
            text (toString error)
