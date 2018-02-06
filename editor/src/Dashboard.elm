port module Dashboard exposing (..)

import Models exposing (..)
import RemoteData exposing (WebData)
import Navigation exposing (Location)
import Api exposing (PollenRockAPI(..))
import View
import Dict


-- MAIN


main : Program JSSettings DashboardModel DashboardMsg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = View.dashboardView
        , update = update
        , subscriptions = subscriptions
        }


initModel : JSSettings -> Route -> DashboardModel
initModel settings route =
    { route = route
    , fsListDirectory = RemoteData.NotAsked
    , settings = toSettingsDict settings
    , watchResponse = RemoteData.NotAsked
    , renderLocation = Nothing
    }


init : JSSettings -> Location -> ( DashboardModel, Cmd DashboardMsg )
init settings location =
    let
        currentRoute =
            parsePath location.pathname

        model =
            initModel settings currentRoute
    in
        case currentRoute of
            DashboardRoute p ->
                ( model, listDirectory p )

            RenderRoute p ->
                ( { model | watchResponse = RemoteData.Loading }
                , Cmd.batch [ renderFile p, watchFile p ]
                )

            SettingsRoute ->
                ( model, Cmd.none )

            NotFoundRoute ->
                ( model, Navigation.load "/dashboard" )



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

        OnDashboardGoBack ->
            ( model, Navigation.back 1 )

        OnDashboardOpenSettings ->
            ( model, Navigation.load "/settings" )

        OnListDirectory data ->
            ( { model | fsListDirectory = data }, Cmd.none )

        OnSettingsChange name val ->
            updateSettings model name val

        OnResetSettings ->
            ( model
            , Cmd.batch
                [ resetSettings ()
                , Navigation.reload
                ]
            )

        OnWatchingFileChanged data ->
            case model.route of
                RenderRoute p ->
                    let
                        newModel =
                            { model | watchResponse = data }
                    in
                        case model.renderLocation of
                            Just loc ->
                                ( newModel, Cmd.batch [ watchFile p, reloadRenderFrame loc ] )

                            Nothing ->
                                ( newModel, Cmd.batch [ watchFile p, renderFile p ] )

                _ ->
                    ( { model | watchResponse = RemoteData.NotAsked }, Cmd.none )

        OnWatchingFileRendered data ->
            case data of
                RemoteData.Success response ->
                    let
                        loc =
                            case response of
                                RenderFailure _ loc ->
                                    loc

                                RenderSuccess loc ->
                                    loc
                    in
                        ( { model | renderLocation = Just loc }, Cmd.none )

                _ ->
                    ( { model | renderLocation = Nothing }, Cmd.none )


updateSettings : DashboardModel -> String -> SettingValue -> ( DashboardModel, Cmd DashboardMsg )
updateSettings model name val =
    let
        newSettings =
            Dict.insert name val model.settings
    in
        case val of
            ValInvalid ->
                let
                    _ =
                        Debug.log "receive invalid value in settings. Ignore"
                in
                    ( model, Cmd.none )

            ValBool b ->
                ( { model | settings = newSettings }, setBoolSettings ( name, b ) )

            ValString s ->
                ( { model | settings = newSettings }, setStringSettings ( name, s ) )

            ValNumber n ->
                ( { model | settings = newSettings }, setNumberSettings ( name, n ) )



-- Command


port setStringSettings : ( String, String ) -> Cmd msg


port setNumberSettings : ( String, Float ) -> Cmd msg


port setBoolSettings : ( String, Bool ) -> Cmd msg


port resetSettings : () -> Cmd msg


port reloadRenderFrame : String -> Cmd msg


listDirectory : String -> Cmd DashboardMsg
listDirectory srcPath =
    Api.get Api.APIfs Api.fsGetResponseDecoder srcPath
        |> RemoteData.sendRequest
        |> Cmd.map OnListDirectory


{-| Watch file changes
-}
watchFile : String -> Cmd DashboardMsg
watchFile path =
    Api.get Api.APIwatch Api.watchResponseDecoder path
        |> RemoteData.sendRequest
        |> Cmd.map OnWatchingFileChanged


renderFile : String -> Cmd DashboardMsg
renderFile path =
    Api.get Api.APIrender Api.renderResponseDecoder path
        |> RemoteData.sendRequest
        |> Cmd.map OnWatchingFileRendered



-- SUBSCRIPTIONS


subscriptions : DashboardModel -> Sub DashboardMsg
subscriptions model =
    Sub.none
