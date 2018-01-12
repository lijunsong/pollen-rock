module Models exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Util


type Route
    = DashboardRoute String
    | EditorRoute String
    | NotFoundRoute


{-| Convert a string url path to corresponding Route. Currently it
supports

  - DashboardRoute: /dashboard/$path
  - EditorRoute: /editor/$path

-}
parsePath : String -> Route
parsePath urlPath =
    case Util.splitUrl urlPath of
        "dashboard" :: rest ->
            DashboardRoute <| Util.concatUrl rest

        "editor" :: rest ->
            EditorRoute <| Util.concatUrl rest

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


type alias DashboardModel =
    { route : Route
    , fsListDirectory : WebData FsGetResponse
    }


type DashboardMsg
    = OnLocationChange Location
    | OnListDirectory (WebData FsGetResponse)
