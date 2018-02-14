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


{-| ask CodeMirror to show the doc
-}
port initDoc : String -> Cmd msg


{-| ask CodeMirror give it's contents back here
-}
port askCMContent : () -> Cmd msg


{-| Notify us when CodeMirror gives us (paired with askCMContent)
-}
port getCMContent : (CodeMirrorContents -> msg) -> Sub msg


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


{-| ask JS to update pollenSetup on the JS side
-}
port updatePollenSetup : PollenSetup -> Cmd msg


{-| ask JS to popup info message. Time is ms
-}
port notifyInfo : ( String, Int ) -> Cmd msg


{-| ask JS to popup error message. Time is ms
-}
port notifyError : ( String, Int ) -> Cmd msg



-- Model


defaultNotifyTimeout : Int
defaultNotifyTimeout =
    3000


{-| initially, we want javascript passes the following value to init:

  - filePath of the editing file

-}
type alias Flags =
    { filePath : String
    }


defaultCommandChar : String
defaultCommandChar =
    "◊"


initPollenSetup : PollenSetup
initPollenSetup =
    { commandChar = defaultCommandChar }


init : Flags -> ( EditorModel, Cmd EditorMsg )
init flags =
    ( { filePath = flags.filePath
      , docState = DocSaved
      , unsavedSeconds = 0
      , layout = Nothing
      , pollenSetup = initPollenSetup
      }
    , Cmd.batch
        [ readFile flags.filePath
        , readConfig flags.filePath
        ]
    )



-- Update


{-| search a tag matching the given name. If no such tag is found,
search the name with a `default-` prefix. If the second search also fails, return Nothing
-}
getTag : String -> List Tag -> Maybe Tag
getTag name tags =
    let
        search name =
            List.head
                (List.filter
                    (\tag ->
                        case tag of
                            VariableTag n v ->
                                n == name

                            ProcedureTag n v ->
                                n == name
                    )
                    tags
                )
    in
        case search name of
            Nothing ->
                search ("default-" ++ name)

            result ->
                result


updateConfig : EditorModel -> List Tag -> EditorModel
updateConfig model tags =
    case getTag "command-char" tags of
        Just (VariableTag _ var) ->
            let
                char =
                    case var of
                        StringVal s ->
                            s

                        CharVal c ->
                            c

                        _ ->
                            defaultCommandChar

                setup =
                    model.pollenSetup

                newSetup =
                    { setup | commandChar = char }
            in
                { model | pollenSetup = newSetup }

        Nothing ->
            let
                _ =
                    Debug.log "commandChar is not found"
            in
                model

        c ->
            let
                _ =
                    Debug.log "incorrect commandChar type" c
            in
                model


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

        OnConfigRead response ->
            case response of
                RemoteData.Success { errno, tags } ->
                    if errno /= 0 then
                        ( model, notifyError ( "Failed to load config", defaultNotifyTimeout ) )
                    else
                        let
                            newModel =
                                updateConfig model tags
                        in
                            ( newModel
                            , Cmd.batch
                                [ updatePollenSetup newModel.pollenSetup
                                , notifyInfo ( "Ready to Rock!", defaultNotifyTimeout )
                                ]
                            )

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

        OnGetCMContent { contents, syntaxCheckResult, generation } ->
            ( { model | docState = DocSaving syntaxCheckResult }
            , writeFile model.filePath contents
            )

        OnFileSaved response ->
            case response of
                RemoteData.Success result ->
                    let
                        { errno, message } =
                            result

                        renderCmd =
                            case ( model.layout, model.docState ) of
                                ( Nothing, _ ) ->
                                    Cmd.none

                                ( _, DocSaving True ) ->
                                    renderFile model.filePath

                                _ ->
                                    Cmd.none

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
            case response of
                RemoteData.Success result ->
                    case result of
                        RenderSuccess location ->
                            ( model, liveView ("/" ++ location) )

                        RenderFailure errno location ->
                            ( model, liveView ("/" ++ location) )

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


readConfig : String -> Cmd EditorMsg
readConfig path =
    Api.get Api.APIconfig Api.tagsResponseDecoder path
        |> RemoteData.sendRequest
        |> Cmd.map OnConfigRead
