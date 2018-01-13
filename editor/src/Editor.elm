port module Editor exposing (..)

import RemoteData exposing (RemoteData, WebData)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Api
import Models exposing (..)
import Time


maxUnsavedSeconds : Int
maxUnsavedSeconds =
    1



-- Main


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- JS Native functions. One Command is paired with one Subscription


{-| get the token under the cursor
-}
port token : (String -> msg) -> Sub msg


{-| ask CodeMirror to show the doc
-}
port initDoc : String -> Cmd msg


{-| ask CodeMirror give it's contents back here
-}
port askCMContent : () -> Cmd msg


{-| Notify us when CodeMirror gives us (paired with askCMContent)
-}
port getCMContent : (String -> msg) -> Sub msg


{-| Notify us when CodeMirror contents are changed
-}
port markContentsDirty : (Int -> msg) -> Sub msg



-- Model


{-| initially, we want javascript passes the following value to init:

  - filePath of the editing file

-}
type alias Flags =
    { filePath : String
    }


init : Flags -> ( EditorModel, Cmd EditorMsg )
init flags =
    ( { filePath = flags.filePath
      , docState = DocSaved
      , unsavedSeconds = 0
      }
    , readFile flags.filePath
    )



-- Update


update : EditorMsg -> EditorModel -> ( EditorModel, Cmd EditorMsg )
update msg model =
    case msg of
        OnFileRead response ->
            case response of
                RemoteData.Success (FileContents s) ->
                    ( { model | docState = DocSaved }, initDoc s )

                RemoteData.Success (FsError code) ->
                    ( { model | docState = DocError }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnTick t ->
            case model.docState of
                DocDirty ->
                    if model.unsavedSeconds >= maxUnsavedSeconds then
                        ( { model | unsavedSeconds = 0 }, askCMContent () )
                    else
                        ( { model | unsavedSeconds = model.unsavedSeconds + 1 }, Cmd.none )

                _ ->
                    ( { model | unsavedSeconds = 0 }, Cmd.none )

        OnGetCMContent contents ->
            ( { model | docState = DocSaving }, writeFile model.filePath contents )

        OnFileSaved response ->
            case response of
                RemoteData.Success { errno, message } ->
                    let
                        state =
                            if errno /= 0 then
                                DocError
                            else
                                DocSaved

                        _ =
                            Debug.log "file saved state" errno
                    in
                        ( { model | docState = state }, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "response" response
                    in
                        ( { model | docState = DocError }, Cmd.none )

        OnCMContentChanged gen ->
            ( { model | docState = DocDirty, unsavedSeconds = 0 }, Cmd.none )



-- Subscriptions


subscriptions : EditorModel -> Sub EditorMsg
subscriptions model =
    Sub.batch
        [ Time.every Time.second OnTick
        , getCMContent OnGetCMContent
        , markContentsDirty OnCMContentChanged
        ]



-- View


view : EditorModel -> Html EditorMsg
view model =
    let
        state =
            case model.docState of
                DocSaving ->
                    "saving"

                DocSaved ->
                    "saved"

                DocError ->
                    "error"

                DocDirty ->
                    ""
    in
        div []
            [ text model.filePath
            , span [ style [ ( "float", "right" ) ] ] [ text (toString model.docState) ]
            ]



-- Command


readFile : String -> Cmd EditorMsg
readFile path =
    Api.get Api.APIfs Api.fsGetResponseDecoder path
        |> RemoteData.sendRequest
        |> Cmd.map OnFileRead


writeFile : String -> String -> Cmd EditorMsg
writeFile path data =
    Api.post Api.APIfs Api.fsPostResponseDecoder [ ( "op", "write" ), ( "data", data ) ] path
        |> RemoteData.sendRequest
        |> Cmd.map OnFileSaved
