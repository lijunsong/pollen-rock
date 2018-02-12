module View.Dashboard exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (..)
import RemoteData exposing (WebData)
import Util


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
    case sourceType path of
        Text ->
            False

        _ ->
            True


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


makeEntry : String -> FolderItem -> Html msg
makeEntry parent item =
    case item of
        Directory name ->
            let
                url =
                    Util.concatUrl [ "/dashboard", parent, name ]

                newName =
                    name ++ "/"
            in
                tr []
                    [ td [] [ a [ href url ] [ text newName ] ]
                    , td [] []
                    , td [] []
                    ]

        File name ->
            let
                editorUrl =
                    Util.concatUrl [ "/editor", parent, name ]

                renderUrl =
                    Util.concatUrl [ "/render", parent, name ]

                editorColumn =
                    if isSupportedSource name then
                        a [ href editorUrl ] [ text "Edit" ]
                    else
                        text ""

                renderColumn =
                    if isSupportedSource name then
                        a [ href renderUrl ] [ text "Render" ]
                    else
                        text ""
            in
                tr []
                    [ td [] [ text name ]
                    , td [] [ editorColumn ]
                    , td [] [ renderColumn ]
                    ]


tabulate : String -> List FolderItem -> Html msg
tabulate path items =
    table [ class "dashboard-list-item" ] (List.map (\i -> makeEntry path i) items)


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
                        tabulate path sortedItems

                -- div [] (List.map (\s -> itemView path s) items)
                FileContents contents ->
                    div []
                        [ text contents ]

                FsError code ->
                    div []
                        [ text ("error code: " ++ (toString code)) ]

        RemoteData.Failure error ->
            text (toString error)
