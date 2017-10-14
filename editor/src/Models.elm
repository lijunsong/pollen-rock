module Models exposing (..)

import RemoteData exposing (WebData)


type alias File =
    { fileName : String
    , fileType : FileType
    }

type FileType
    = Directory
    | RegularFile

type alias Model =
    { folderContents : WebData (List File)
    }

initialModel : Model
initialModel =
    { folderContents = RemoteData.Loading
    }
