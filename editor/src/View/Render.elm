module View.Render exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Util


view : WebData WatchResponse -> Maybe String -> Html DashboardMsg
view response location =
    case location of
        Just loc ->
            div [ class "frameContainer" ] [ iframe [ src <| "/" ++ loc, id "renderFrame" ] [] ]

        _ ->
            case response of
                RemoteData.NotAsked ->
                    text "Not asked"

                RemoteData.Loading ->
                    text "Loading"

                RemoteData.Failure _ ->
                    text "Server failure"

                RemoteData.Success (WatchingFileChanged mtime) ->
                    text "Pollen Rock internal error in Render page"

                RemoteData.Success WatchingFileNotExists ->
                    text "File not exist"
