module Models exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Util
import Time
import Dict


type Route
    = DashboardRoute String
    | RenderRoute String
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

        "render" :: rest ->
            RenderRoute <| Util.concatUrl rest

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
    | RenderFailure Int String


type WatchResponse
    = WatchingFileChanged Int
    | WatchingFileNotExists



-- Response to Tags and Config


type Variable
    = NumberVal Float
    | BooleanVal Bool
    | StringVal String
    | CharVal String
    | SymbolVal String
    | UnknownVal


type ProcedureKeywords
    = KeywordsAny
    | KeywordsList (List String)


type alias Procedure =
    { arity : Int
    , arityAtLeast : Bool
    , allKeywords : ProcedureKeywords
    , requiredKeywords : List String
    }


type Tag
    = VariableTag String Variable
    | ProcedureTag String Procedure


type alias TagsResponse =
    { errno : Int
    , tags : List Tag
    }


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
            , ( ".pp", Pollen "pp" )
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
    , watchResponse : WebData WatchResponse
    , renderLocation : Maybe String -- watch page's rendering location
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
    | OnWatchingFileChanged (WebData WatchResponse)
    | OnWatchingFileRendered (WebData RenderResponse) -- occurs only once
    | OnListDirectory (WebData FsGetResponse)
    | OnSettingsChange String SettingValue
    | OnResetSettings


{-| the state of the document in Elm's view. DocSaving takes an extra
parameter to indicate whether the document is correct in Syntax. True
means correct, False otherwise.
-}
type DocState
    = DocSaving Bool
    | DocSaved
    | DocError
    | DocDirty


stateToText : DocState -> String
stateToText state =
    case state of
        DocSaving _ ->
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


{-| TODO: Make another dict type for tag response (key is identifier
from the pollen setup moduel) because pollen setup contains
identifiers that have '-' in the name. Like what we did to Settings
-}
type alias PollenSetup =
    { commandChar : String
    }


{-| Pollen Editor model. It tracks status of the editor UI

  - filePath is used to save the contents.
  - docState is used to decide if the editor needs to write the doc to
    the server.
  - secondCounter counts the seconds, which is reset to 0 whenever user
    types anything. This is used to implement saving documents N seconds
    after user stops typing.

-}
type alias EditorModel =
    { filePath : String
    , docState : DocState
    , unsavedSeconds : Int
    , layout : Maybe EditorLayout
    , pollenSetup : PollenSetup
    }


{-| CodeMirror status. Because Elm does all the work to save and
render thesource code, this data structure is what Elm uses to
communicate with JS
-}
type alias CodeMirrorContents =
    { contents : String
    , syntaxCheckResult : Bool
    , generation : Int
    }


type EditorMsg
    = OnFileRead (WebData FsGetResponse)
    | OnConfigRead (WebData TagsResponse)
    | OnEditorGoBack
    | OnEditorOpenSettings
    | OnTick Time.Time
    | OnGetCMContent CodeMirrorContents
    | OnFileSaved (WebData FsPostResponse)
    | OnCMContentChanged Int
    | OnRendered (WebData RenderResponse)
    | OnLayoutChange (Maybe EditorLayout)
