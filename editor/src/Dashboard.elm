module Dashboard exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Api exposing (PollenRockAPI(..))
import View.Dashboard


-- MAIN


main : Program Never DashboardModel DashboardMsg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = View.Dashboard.view
        , update = update
        , subscriptions = subscriptions
        }


initialModel : Route -> DashboardModel
initialModel route =
    { route = route
    , fsListDirectory = RemoteData.NotAsked
    }


init : Location -> ( DashboardModel, Cmd DashboardMsg )
init location =
    let
        currentRoute =
            parsePath location.pathname

        msg =
            case currentRoute of
                DashboardRoute p ->
                    listDirectory p

                _ ->
                    Navigation.load "/dashboard"
    in
        ( initialModel currentRoute, msg )



-- Updates


update : DashboardMsg -> DashboardModel -> ( DashboardModel, Cmd DashboardMsg )
update msg model =
    case msg of
        OnLocationChange location ->
            let
                newRoute =
                    parsePath location.pathname
            in
                ( { model | route = newRoute }, Cmd.none )

        OnListDirectory data ->
            ( { model | fsListDirectory = data }, Cmd.none )



-- Command


listDirectory : String -> Cmd DashboardMsg
listDirectory srcPath =
    Api.get Api.APIfs Api.fsGetResponseDecoder srcPath
        |> RemoteData.sendRequest
        |> Cmd.map OnListDirectory



-- SUBSCRIPTIONS


subscriptions : DashboardModel -> Sub DashboardMsg
subscriptions model =
    Sub.none
