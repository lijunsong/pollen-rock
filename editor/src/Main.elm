module Main exposing (..)

import Html exposing (Html, program, div, text, a)
import Html.Attributes exposing (href)

import Navigation exposing (Location)
-- import RemoteData exposing (WebData)

-- import UrlParser as Url
-- import UrlParser exposing ((</>), Parser, oneOf, s)
-- import UrlParser as Url exposing ((</>), (<?>), s, string, stringParam, top)
import RemoteData exposing (WebData)

import Http
import Json.Decode as Json

-- Models

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


initialModel: Route -> Model
initialModel route =
    { route = route
    , fsContents = RemoteData.NotAsked
    , fsOpAnswer = RemoteData.NotAsked
    }


-- Routing
splitUrl : String -> List String
splitUrl url =
  case String.split "/" url of
    "" :: segments ->
      segments

    segments ->
      segments

{-|

/fs/path1/path2/path3/<folder>s
/editor/path1/path2/path3/<pollen-source>

-}
parsePath : String -> Route
parsePath urlPath =
    case splitUrl urlPath of
        "fs" :: rest -> IndexRoute (String.join "/" rest)
        "editor" :: rest -> EditorRoute (String.join "/" rest)
        _ -> NotFoundRoute

parseLocation : Location -> Route
parseLocation location =
    parsePath location.pathname


-- Messages
type Msg
    = OnLocationChange Location
    | OnListFolder (WebData FsContentsAnswer)
    | OnMoveItem (WebData FsOpAnswer)


-- Commands
-- encode http://pietrograndi.com/porting-an-api-service-from-js-to-elm/

-- {errno, message}
fsOpAnswerDecoder =
    Json.map2 FsOpAnswer
        (Json.field "errno" Json.int)
        (Json.field "message" Json.string)

-- {errno, items} or {errno, contents}
folderItemDecoder =
    Json.string
        |> Json.andThen
           (\str ->
                if String.endsWith "/" str then
                    Json.succeed (Directory str)
                else
                    Json.succeed (File str))

fsGetErrorDecoder =
    Json.field "errno" Json.int
        |> Json.andThen
           (\errno ->
                if errno /= 0 then
                    Json.succeed (FsError errno)
                else
                    Json.fail "fsGet return 0")

fsGetAnswerDecoder =
    Json.oneOf [
         Json.map FolderContents (Json.field "items" (Json.list folderItemDecoder)),
         Json.map FileContents (Json.field "contents" (Json.string)),
         fsGetErrorDecoder
        ]

urlBase = "http://ttybook.local:8000"
apiUrl = urlBase ++ "/rest"

encodeParams : (List (String, String)) -> String
encodeParams kvList =
    let encodeParam (k, v) =
            Http.encodeUri k ++ "=" ++ Http.encodeUri v
    in
        String.join "&"
            (List.map encodeParam kvList)

apiPost whichAPI body decoder srcPath =
    Http.post (String.join "/" [apiUrl, whichAPI, srcPath])
        (Http.stringBody
             "application/x-www-form-urlencoded"
             (encodeParams body))
        decoder

apiGet whichAPI decoder srcPath =
    Http.get (Debug.log "Wowo "(String.join "/" [apiUrl, whichAPI, srcPath])) decoder


apiFsPost = apiPost "fs"
apiFsGet = apiGet "fs"

apiFsMv src dst =
    apiFsPost [("op", "mv"), ("data", dst)] fsOpAnswerDecoder src

apiFsLs = apiFsGet fsGetAnswerDecoder


ls : String -> Cmd Msg
ls src =
    Debug.log "ls " (apiFsLs src)
        |> RemoteData.sendRequest
        |> Cmd.map OnListFolder

mv : String -> String -> Cmd Msg
mv src dst =
    apiFsMv src dst
        |> RemoteData.sendRequest
        |> Cmd.map OnMoveItem

-- Updates
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OnLocationChange location ->
            let newRoute = parseLocation location in
            ( { model | route = newRoute }, Cmd.none )

        OnListFolder data ->
            ( { model | fsContents = data }, Cmd.none )

        OnMoveItem data ->
            ( { model | fsOpAnswer = data }, Cmd.none )


-- view

folderView : Model -> Html msg
folderView model =
    let itemView item =
            case item of
                Directory name ->
                    -- it should get model's route and append it here!
                    a [href ("/fs/" ++ name)] [text name]
                File name -> text name
    in
    case model.fsContents of
        RemoteData.NotAsked ->
            text "Not asked"
        RemoteData.Loading ->
            text "Loading"
        RemoteData.Success (FolderContents items) ->
            div [] (List.map (\item -> div [] [itemView item]) items)
        RemoteData.Success (FileContents contents) ->
            div [] [ text "File Contents"
                   , text contents ]
        RemoteData.Success (FsError errno) ->
            div [] [ text "op error."
                   , text ("errno is " ++ (toString errno)) ]
        RemoteData.Failure error ->
            text (toString error)

view : Model -> Html Msg
view model =
    let loc =
        case model.route of
            IndexRoute loc -> "index " ++ loc
            EditorRoute loc -> "editor " ++ loc
            NotFoundRoute -> "not found"
        contents = folderView model
    in
    div []
        [ div [] [text loc]
        , div [] [contents]
        ]

-- MAIN

init : Location -> ( Model, Cmd Msg )
init location =
    let currentRoute = parseLocation location
        path =
            case currentRoute of
                IndexRoute p -> p
                EditorRoute p -> "NYI-editor"
                NotFoundRoute -> "NYI-NFR"
    in
        (initialModel currentRoute, ls (Debug.log "path: " path ))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
