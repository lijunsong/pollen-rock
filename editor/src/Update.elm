module Update exposing (..)

import Msgs exposing (Msg)
import Models exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msgs.OnListFolder response ->
            ( { model | folderContents = response }, Cmd.none )
