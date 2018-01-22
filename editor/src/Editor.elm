port module Editor exposing (..)

import RemoteData exposing (RemoteData, WebData)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Api
import Models exposing (..)
import Time
import View
import Navigation


maxUnsavedSeconds : Int
maxUnsavedSeconds =
    1



-- Main


main =
    Html.programWithFlags
        { init = init
        , view = View.editorView
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


{-| ask JS to allow browser close
-}
port allowClose : Bool -> Cmd msg


{-| ask the browser to load the path in the iframe/webkit
-}
port liveView : String -> Cmd msg


{-| ask JS to change layout
-}
port changeLayout : String -> Cmd msg


{-| ask JS to update code mirror option
-}
port setCMOption : ( String, String ) -> Cmd msg



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
      , layout = Nothing
      }
    , readFile flags.filePath
    )



-- Update


update : EditorMsg -> EditorModel -> ( EditorModel, Cmd EditorMsg )
update msg model =
    case msg of
        OnFileRead response ->
            let
                fileMode =
                    case sourceType model.filePath of
                        Pollen _ ->
                            "pollenMixed"

                        Xml ->
                            "xml"

                        Racket ->
                            "scheme"

                        _ ->
                            "null"

                initEditor contents =
                    Cmd.batch
                        [ initDoc contents
                        , setCMOption ( "mode", fileMode )
                        ]
            in
                case response of
                    RemoteData.Success (FileContents s) ->
                        ( { model | docState = DocSaved }, initEditor s )

                    RemoteData.Success (FsError code) ->
                        let
                            _ =
                                Debug.log "code" code
                        in
                            ( { model | docState = DocError }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

        OnEditorGoBack ->
            ( model, Navigation.back 1 )

        OnEditorOpenSettings ->
            ( model, Navigation.load "/settings" )

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
                RemoteData.Success result ->
                    let
                        { errno, message } =
                            result

                        renderCmd =
                            case model.layout of
                                Nothing ->
                                    Cmd.none

                                _ ->
                                    renderFile model.filePath

                        allCmd =
                            Cmd.batch [ renderCmd, allowClose True ]
                    in
                        case errno of
                            0 ->
                                ( { model | docState = DocSaved }, allCmd )

                            _ ->
                                ( { model | docState = DocError }, allowClose True )

                _ ->
                    let
                        _ =
                            Debug.log "response" response
                    in
                        ( { model | docState = DocError }, allowClose True )

        OnCMContentChanged gen ->
            ( { model | docState = DocDirty, unsavedSeconds = 0 }, allowClose False )

        OnRendered response ->
            let
                _ =
                    Debug.log "render response" response
            in
                case response of
                    RemoteData.Success result ->
                        case result of
                            RenderSuccess location ->
                                ( model, liveView ("/" ++ location) )

                            RenderFailure errno ->
                                ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

        OnLayoutChange layout ->
            let
                newModel =
                    { model | layout = layout }

                cmd =
                    case layout of
                        Nothing ->
                            changeLayout "close"

                        Just HorizontalLayout ->
                            changeLayout "horizontal"

                        Just VerticalLayout ->
                            changeLayout "vertical"
            in
                ( newModel, Cmd.batch [ cmd, renderFile model.filePath ] )



-- Subscriptions


subscriptions : EditorModel -> Sub EditorMsg
subscriptions model =
    Sub.batch
        [ Time.every Time.second OnTick
        , getCMContent OnGetCMContent
        , markContentsDirty OnCMContentChanged
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


renderFile : String -> Cmd EditorMsg
renderFile path =
    Api.get Api.APIrender Api.renderResponseDecoder path
        |> RemoteData.sendRequest
        |> Cmd.map OnRendered
