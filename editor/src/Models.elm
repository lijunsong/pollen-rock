module Models exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Util
import Time


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


{-| CodeMirror settings
-}
type alias Settings =
    { lineNumbers : Bool
    , lineWrapping : Bool
    }


{-| The model of Dashboard. It includes the model of general settings
-}
type alias DashboardModel =
    { route : Route
    , fsListDirectory : WebData FsGetResponse
    , settings : Settings
    }


type DashboardMsg
    = OnLocationChange Location
    | OnDashboardGoBack
    | OnListDirectory (WebData FsGetResponse)
    | OnSettingsLineNumberChange


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


type alias EditorModel =
    { filePath : String
    , docState : DocState
    , unsavedSeconds : Int
    }


type EditorMsg
    = OnFileRead (WebData FsGetResponse)
    | OnEditorGoBack
    | OnTick Time.Time
    | OnGetCMContent String
    | OnFileSaved (WebData FsPostResponse)
    | OnCMContentChanged Int
    | Render
    | OnRendered (WebData RenderResponse)
