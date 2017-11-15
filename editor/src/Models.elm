module Models exposing (..)

import RemoteData exposing (WebData)
import Navigation exposing (Location)


type Route
    = IndexRoute String
    | EditorRoute String
    | NotFoundRoute


type FolderItem
    = Directory String
    | File String


{-|
Response of GET of /fs/$path
-}
type FsContentsAnswer
    = FolderContents (List FolderItem)
    | FileContents String
    | FsError Int


{-|
Response of filesystem operation  POST to /fs/$path
-}
type alias FsOpAnswer =
    { errno: Int
    , message: String
    }


type alias Model =
    { route: Route
    , fsContents: (WebData FsContentsAnswer)
    , fsOpAnswer: (WebData FsOpAnswer)
    }

-- Messages
type Msg
    = OnLocationChange Location
    | OnListFolder (WebData FsContentsAnswer)
    | OnMoveItem (WebData FsOpAnswer)
