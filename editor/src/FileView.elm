module FileView exposing (..)

import Models exposing (..)
import Html exposing (Html, div, ul, li, text, i)
import Html.Attributes exposing (class)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Icons


view : Model -> Html Msg
view model =
    div []
        [page model.folderContents]

page : WebData (List File) -> Html Msg
page response =
    div []
        [ maybeList response ]

maybeList : WebData (List File) -> Html Msg
maybeList response =
    case response of
        RemoteData.NotAsked ->
            text ""
        RemoteData.Loading ->
            text "loading"
        RemoteData.Success contents ->
            list contents
        RemoteData.Failure error ->
            text (toString error)


list : (List File) -> Html Msg
list contents =
    div [ class "max-width-2 mx-auto" ]
        [ viewFolders contents
        , viewFiles contents
        ]


viewFileType : FileType -> (File -> Html Msg) -> (List File) -> Html Msg
viewFileType ty func contents =
    let files = List.filter (\f -> f.fileType == ty) contents in
    div [class "p2 border rounded"]
        (List.map func files)

viewFolders : (List File) -> Html Msg
viewFolders =
    viewFileType Directory viewFolder


viewFolder : File -> Html Msg
viewFolder f =
    li [class "flex"]
        [ div [class "flex-auto"] [text f.fileName]
        , Icons.fa "pencil"
        , Icons.fa "trash"
        ]


viewFiles : (List File) -> Html Msg
viewFiles =
    viewFileType RegularFile viewRegularFile


viewRegularFile : File -> Html Msg
viewRegularFile f =
    li [class "flex"]
        [ div [class "flex-auto"] [text f.fileName]
        , Icons.fa "code"
        , Icons.fa "eye"
        , Icons.fa "pencil"
        , Icons.fa "trash"
        ]
