module FileView exposing (..)

import Models exposing (..)
import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (class)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)


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
    div [ class "files" ]
        [ ul []
          (List.map viewFile contents)]

viewFile : File -> Html Msg
viewFile f =
    li []
       [text f.fileName, text (toString f.fileType)]
