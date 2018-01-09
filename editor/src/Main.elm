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
    , pollenQueryResponse = RemoteData.NotAsked
    }


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


parseLocation : Location -> Route
parseLocation location =
    parsePath location.pathname



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "location change: " msg
    in
        case msg of
            OnLocationChange location ->
                let
                    newRoute =
                        parseLocation location
                in
                    ( { model | route = newRoute }, Cmd.none )

            OnPollenResponseReceive data ->
                ( { model | pollenQueryResponse = data }, Cmd.none )



-- MAIN


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location

        msg =
            case currentRoute of
                DashboardRoute p ->
                    Api.listDirectory p

                EditorRoute p ->
                    let
                        _ =
                            Debug.log "init editor route NYI" p
                    in
                        Cmd.none

                NotFoundRoute ->
                    Navigation.load "/dashboard"
    in
        ( initialModel currentRoute, msg )


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
