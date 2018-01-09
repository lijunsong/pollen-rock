module Models exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)


type Route
    = DashboardRoute String
    | EditorRoute String
    | NotFoundRoute


routerResourceString : Route -> String
routerResourceString route =
    case route of
        DashboardRoute _ ->
            "dashboard"

        EditorRoute _ ->
            "editor"

        _ ->
            "404"


type FolderItem
    = Directory String
    | File String



-- Response to filesystem query


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


{-| Pollen-rock response type. This type contains all reponse types
from the Pollen-rock server.
-}
type PollenQueryResponse
    = FsGet FsGetResponse
    | FsPost FsPostResponse



-- Model


type alias Model =
    { route : Route
    , pollenQueryResponse : WebData PollenQueryResponse
    }



-- Messages


type Msg
    = OnLocationChange Location
    | OnPollenResponseReceive (WebData PollenQueryResponse)
