module View exposing (view)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (Html, program, div, text, a)
import Html.Attributes exposing (href, class)
import Util


indexPage : String -> Model -> Html Msg
indexPage path model =
    div [ class "clearfix" ]
        [ div [ class "sm-col-12 md-col-8 lg-col-4 mx-auto" ]
            [ nav path
            , list path model
            ]
        ]


nav : String -> Html Msg
nav path =
    let
        contents =
            if String.isEmpty path then
                "Home"
            else
                path
    in
        div [ class "p1 header" ] [ text contents ]

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
        div [ class "p1 light-border border-bottom" ]
            [ row ]


list : String -> Model -> Html Msg
list path model =
    case model.fsContents of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            text "Loading"

        RemoteData.Success (FolderContents items) ->
            div []
                (List.map (\item -> itemView path item) items)

        RemoteData.Success (FileContents contents) ->
            div []
                [ text "File Contents"
                , text contents
                ]

        RemoteData.Success (FsError errno) ->
            div []
                [ text "op error."
                , text ("errno is " ++ (toString errno))
                ]

        RemoteData.Failure error ->
            text (toString error)


view : Model -> Html Msg
view model =
    case model.route of
        IndexRoute loc ->
            indexPage loc model

        EditorRoute loc ->
            div [] [ text "editor route NYI" ]

        NotFoundRoute ->
            div [] [ text "not found" ]
