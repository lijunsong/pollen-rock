module Models exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Util
import Time
import Dict


type Route
    = DashboardRoute String
    | SettingsRoute
    | NotFoundRoute


{-| Convert a string url path to corresponding Route. Currently it
supports

  - DashboardRoute: /dashboard/$path
  - EditorSettingsRoute: /editor/$path

-}
parsePath : String -> Route
parsePath urlPath =
    case Util.splitUrl urlPath of
        "dashboard" :: rest ->
            DashboardRoute <| Util.concatUrl rest

        "settings" :: [] ->
            SettingsRoute

        _ ->
            NotFoundRoute



-- Response to filesystem query


type FolderItem
    = Directory String
    | File String


{-| Response of GET of /fs/$path. If `$path` is a folder on disk, the
answer is FolderContents. If a file, the answer is FileContents.
-}
type FsGetResponse
    = FolderContents (List FolderItem)
    | FileContents String
    | FsError Int


{-| Response of POST to /fs/$path
-}
type alias FsPostResponse =
    { errno : Int
    , message : String
    }



-- Response to Render


{-| Response of GET to /render/$path
-}
type RenderResponse
    = RenderSuccess String
    | RenderFailure Int


{-| Source type that the editor should support
-}
type SourceCodeMode
    = Pollen String
    | Racket
    | Xml
    | Text


sourceType : String -> SourceCodeMode
sourceType filePath =
    let
        match list =
            case list of
                [] ->
                    Text

                ( s, mode ) :: tl ->
                    if String.endsWith s filePath then
                        mode
                    else
                        match tl
    in
        match
            [ ( ".pm", Pollen "pm" )
            , ( ".p", Pollen "p" )
            , ( ".html", Xml )
            , ( ".rkt", Racket )
            ]


{-| CodeMirror settings. SettingsDict is what PollenRock uses to
maintain the model of settings. JSSettings is what Javascript passes
into Elm initially (it must be a record to communicate between Elm and
JS via a port). So Elm takes flags that is JSSettings, and converts
that into SettingsDict when app starts.
-}
type SettingValue
    = ValBool Bool
    | ValString String
    | ValNumber Float
    | ValInvalid


type alias SettingsDict =
    Dict.Dict String SettingValue


type alias JSSettings =
    { lineNumbers : Bool
    , lineWrapping : Bool
    }


toSettingsDict : JSSettings -> SettingsDict
toSettingsDict jsSettings =
    Dict.fromList
        [ ( "lineNumbers", jsSettings.lineNumbers |> ValBool )
        , ( "lineWrapping", jsSettings.lineWrapping |> ValBool )
        ]


{-| The model of Dashboard. It includes the model of general settings
-}
type alias DashboardModel =
    { route : Route
    , fsListDirectory : WebData FsGetResponse
    , settings : SettingsDict
    }


{-| Msg for /dashboard and /settings. There are also two location
events in this message. I can make them links, but I choose not
because of the complexity in css.
-}
type DashboardMsg
    = OnLocationChange Location
    | OnDashboardGoBack
    | OnDashboardOpenSettings
    | OnListDirectory (WebData FsGetResponse)
    | OnSettingsChange String SettingValue
    | OnResetSettings


{-| The model for editor

  - filePath is used to save the contents.
  - docState is used to decide if the editor needs to write the doc to
    the server.
  - secondCounter counts the seconds, which is reset to 0 whenever user
    types anything. This is used to implement saving documents N seconds
    after user stops typing.

-}
type DocState
    = DocSaving
    | DocSaved
    | DocError
    | DocDirty


stateToText : DocState -> String
stateToText state =
    case state of
        DocSaving ->
            "saving"

        DocSaved ->
            "saved"

        DocError ->
            "error"

        DocDirty ->
            ""


type EditorLayout
    = HorizontalLayout
    | VerticalLayout


type alias EditorModel =
    { filePath : String
    , docState : DocState
    , unsavedSeconds : Int
    , layout : Maybe EditorLayout
    }


type EditorMsg
    = OnFileRead (WebData FsGetResponse)
    | OnEditorGoBack
    | OnEditorOpenSettings
    | OnTick Time.Time
    | OnGetCMContent String
    | OnFileSaved (WebData FsPostResponse)
    | OnCMContentChanged Int
    | OnRendered (WebData RenderResponse)
    | OnLayoutChange (Maybe EditorLayout)
