module Msgs exposing (..)

import Models exposing (File)
import RemoteData exposing (WebData)

type Msg
    = OnListFolder (WebData (List File))
