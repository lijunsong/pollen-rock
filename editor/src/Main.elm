module Main exposing (..)

import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Models exposing (..)
import Api
import View
import Util


initialModel : Route -> Model
initialModel route =
    { route = route
    , fsContents = RemoteData.Loading
    , fsOpAnswer = RemoteData.Loading
    }


{-| /fs/path1/path2/path3/<folder>s
/editor/path1/path2/path3/<pollen-source>
-}
parsePath : String -> Route
parsePath urlPath =
    case Util.splitUrl urlPath of
        "fs" :: rest ->
            IndexRoute <| Util.concatUrl rest

        "editor" :: rest ->
            EditorRoute <| Util.concatUrl rest

        _ ->
            NotFoundRoute


parseLocation : Location -> Route
parseLocation location =
    {- let
           x =
               Debug.log "location" location
       in
    -}
    parsePath location.pathname



-- Commands
-- {errno, message}
-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        OnListFolder data ->
            ( { model | fsContents = data }, Cmd.none )

        OnMoveItem data ->
            ( { model | fsOpAnswer = data }, Cmd.none )



-- MAIN


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location

        path =
            case currentRoute of
                IndexRoute p ->
                    p

                EditorRoute p ->
                    "NYI-editor"

                NotFoundRoute ->
                    "NYI-NFR"
    in
        ( initialModel currentRoute, Api.ls path )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }
