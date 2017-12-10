module Api exposing (ls, mv)

{-| encode <http://pietrograndi.com/porting-an-api-service-from-js-to-elm/>

This file defines functions that send http request and convert the
response to Elm structure.

It defines Command in this app.

-}

import Models exposing (..)
import Json.Decode as Json
import Http
import RemoteData exposing (WebData)


{-| fs operation (mv, rm, mkdir) answers contain errno and message.
-}
fsOpAnswerDecoder : Json.Decoder FsOpAnswer
fsOpAnswerDecoder =
    Json.map2 FsOpAnswer
        (Json.field "errno" Json.int)
        (Json.field "message" Json.string)


{-| Response decoder of GET (a directory).

`/` will be dropped from directory name

-}
folderItemDecoder : Json.Decoder FolderItem
folderItemDecoder =
    Json.string
        |> Json.andThen
            (\str ->
                if String.endsWith "/" str then
                    Json.succeed (Directory <| String.dropRight 1 str)
                else
                    Json.succeed (File str)
            )


{-| Response decoder of GET when server can't fullfil the request.
-}
fsGetErrorDecoder : Json.Decoder FsContentsAnswer
fsGetErrorDecoder =
    Json.field "errno" Json.int
        |> Json.andThen
            (\errno ->
                if errno /= 0 then
                    Json.succeed (FsError errno)
                else
                    Json.fail "fsGet return 0"
            )


{-| Decoder of GET /fs/$path.
-}
fsGetAnswerDecoder : Json.Decoder FsContentsAnswer
fsGetAnswerDecoder =
    Json.oneOf
        [ Json.map FolderContents (Json.field "items" (Json.list folderItemDecoder))
        , Json.map FileContents (Json.field "contents" (Json.string))
        , fsGetErrorDecoder
        ]


urlBase : String
urlBase =
    "http://localhost:8000"


apiUrl : String
apiUrl =
    urlBase ++ "/rest"


encodeParams : List ( String, String ) -> String
encodeParams kvList =
    let
        encodeParam ( k, v ) =
            Http.encodeUri k ++ "=" ++ Http.encodeUri v
    in
        String.join "&"
            (List.map encodeParam kvList)


apiPost :
    String
    -> List ( String, String )
    -> Json.Decoder a
    -> String
    -> Http.Request a
apiPost whichAPI body decoder srcPath =
    Http.post (String.join "/" [ apiUrl, whichAPI, srcPath ])
        (Http.stringBody
            "application/x-www-form-urlencoded"
            (encodeParams body)
        )
        decoder


apiGet : String -> Json.Decoder a -> String -> Http.Request a
apiGet whichAPI decoder srcPath =
    Http.get (String.join "/" [ apiUrl, whichAPI, srcPath ]) decoder


apiFsPost :
    List ( String, String )
    -> Json.Decoder a
    -> String
    -> Http.Request a
apiFsPost =
    apiPost "fs"


apiFsGet : Json.Decoder a -> String -> Http.Request a
apiFsGet =
    apiGet "fs"


apiFsMv : String -> String -> Http.Request FsOpAnswer
apiFsMv src dst =
    apiFsPost [ ( "op", "mv" ), ( "data", dst ) ] fsOpAnswerDecoder src


apiFsLs : String -> Http.Request FsContentsAnswer
apiFsLs =
    apiFsGet fsGetAnswerDecoder


ls : String -> Cmd Msg
ls src =
    apiFsLs src
        |> RemoteData.sendRequest
        |> Cmd.map OnListFolder


mv : String -> String -> Cmd Msg
mv src dst =
    apiFsMv src dst
        |> RemoteData.sendRequest
        |> Cmd.map OnMoveItem
