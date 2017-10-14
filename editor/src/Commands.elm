module Commands exposing (..)

import Http
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Msgs exposing (Msg)
import Models exposing (File, FileType)
import RemoteData

listFolderUrl : String
listFolderUrl = "http://ttybook.local:4000/files-api"

listFolder : Cmd Msg
listFolder =
    Http.get listFolderUrl (Json.list fileDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnListFolder


fileDecoder : Json.Decoder File
fileDecoder =
    decode File
        |> required "fileName" Json.string
        |> required "fileType" fileTypeDecoder


fileTypeDecoder : Json.Decoder FileType
fileTypeDecoder =
    Json.string
        |> Json.andThen
           (\str ->
                case str of
                    "d" ->
                        Json.succeed Models.Directory
                    "f" ->
                        Json.succeed Models.RegularFile
                    els ->
                        Json.fail <| "Unknown fileType: " ++ els
           )
