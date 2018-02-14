module Api
    exposing
        ( get
        , post
        , PollenRockAPI(..)
        , fsGetResponseDecoder
        , fsPostResponseDecoder
        , renderResponseDecoder
        , watchResponseDecoder
        , tagsResponseDecoder
        )

{-| encode <http://pietrograndi.com/porting-an-api-service-from-js-to-elm/>

This file defines functions that send http request and convert the
response to Elm structure.

It defines Command in this app.

-}

import Models exposing (..)
import Json.Decode as Json
import Http
import Util


-- Decoders: This section defines decoders for all Pollen-rock server
-- json response. These decoders follows Pollen-rock RESTful API
-- specification


{-| The decoder to convert Json response of /rest/fs POST request to
FsPostResponse
-}
fsPostResponseDecoder : Json.Decoder FsPostResponse
fsPostResponseDecoder =
    Json.map2 FsPostResponse
        (Json.field "errno" Json.int)
        (Json.field "message" Json.string)


{-| One of many decoders for FsGetResponse. This one decodes
response when the resource is a directory. Note that `/` will be
dropped from directory name.

Also see @docs fsGetResponseDecoder

-}
fsGetResponseFolderItemDecoder : Json.Decoder FolderItem
fsGetResponseFolderItemDecoder =
    Json.string
        |> Json.andThen
            (\str ->
                if String.endsWith "/" str then
                    Json.succeed (Directory <| String.dropRight 1 str)
                else
                    Json.succeed (File str)
            )


{-| One of many decoders for FsGetResponse. This one decodes
response when request error occurs.

Also see @docs fsGetResponseDecoder

-}
fsGetResponseErrorDecoder : Json.Decoder FsGetResponse
fsGetResponseErrorDecoder =
    Json.field "errno" Json.int
        |> Json.andThen
            (\errno ->
                if errno /= 0 then
                    Json.succeed (FsError errno)
                else
                    Json.fail "fsGet return 0"
            )


{-| The decoder to convert Json response of /rest/fs GET request to
FsGetResponse
-}
fsGetResponseDecoder : Json.Decoder FsGetResponse
fsGetResponseDecoder =
    Json.oneOf
        [ Json.map FolderContents
            (Json.field "items" (Json.list fsGetResponseFolderItemDecoder))
        , Json.map FileContents (Json.field "contents" (Json.string))
        , fsGetResponseErrorDecoder
        ]


{-| The decoder to convert Json response of /rest/render GET request to
RenderResponse
-}
renderResponseDecoder : Json.Decoder RenderResponse
renderResponseDecoder =
    Json.field "errno" Json.int
        |> Json.andThen
            (\errno ->
                if errno /= 0 then
                    Json.map (RenderFailure errno) (Json.field "location" Json.string)
                else
                    Json.map RenderSuccess (Json.field "location" Json.string)
            )


{-| The decoder to convert Json response of /rest/watch GET request to
WatchResponse
-}
watchResponseDecoder : Json.Decoder WatchResponse
watchResponseDecoder =
    Json.field "errno" Json.int
        |> Json.andThen
            (\errno ->
                if errno /= 0 then
                    Json.succeed WatchingFileNotExists
                else
                    Json.map WatchingFileChanged (Json.field "mtime" Json.int)
            )


{-| A helper decoder for Variable tag
-}
variableDecoder : Json.Decoder Variable
variableDecoder =
    Json.field "type" (Json.nullable Json.string)
        |> Json.andThen
            (\typ ->
                case typ of
                    Nothing ->
                        Json.succeed UnknownVal

                    Just "string" ->
                        Json.map StringVal (Json.field "value" Json.string)

                    Just "char" ->
                        Json.map CharVal (Json.field "value" Json.string)

                    Just "boolean" ->
                        Json.map BooleanVal (Json.field "value" Json.bool)

                    Just "number" ->
                        Json.map NumberVal (Json.field "value" Json.float)

                    Just "symbol" ->
                        Json.map SymbolVal (Json.field "value" Json.string)

                    _ ->
                        Json.succeed UnknownVal
            )


{-| A helper decoder for keywords field of Procedure tag
-}
keywordsDecoder : Json.Decoder ProcedureKeywords
keywordsDecoder =
    Json.oneOf
        [ Json.field "all-keywords" Json.bool |> Json.andThen (\b -> Json.succeed KeywordsAny)
        , Json.map KeywordsList (Json.field "all-keywords" (Json.list Json.string))
        ]


{-| decoder for procedure tag
-}
procedureDecoder : Json.Decoder Procedure
procedureDecoder =
    Json.map4 Procedure
        (Json.field "arity" Json.int)
        (Json.field "arity-at-least" Json.bool)
        (Json.field "all-keywords" keywordsDecoder)
        (Json.field "required-keywords" (Json.list Json.string))


{-| decoder for pollen tag
-}
tagDecoder : Json.Decoder Tag
tagDecoder =
    let
        decodeKind kind =
            case kind of
                "variable" ->
                    Json.map2 VariableTag
                        (Json.field "name" Json.string)
                        variableDecoder

                "procedure" ->
                    Json.map2 ProcedureTag
                        (Json.field "name" Json.string)
                        procedureDecoder

                _ ->
                    Json.fail ("unknown kind " ++ kind)
    in
        Json.field "kind" Json.string
            |> Json.andThen decodeKind


{-| decoder for pollen /rest/tags/$path response
-}
tagsResponseDecoder : Json.Decoder TagsResponse
tagsResponseDecoder =
    Json.map2 TagsResponse
        (Json.field "errno" Json.int)
        (Json.field "tags" (Json.list tagDecoder))



-- High level APIs


{-| According to Pollen-rock docs, Pollen-rock server supports the
following APIs
-}
type PollenRockAPI
    = APIfs
    | APItags
    | APIconfig
    | APIwatch
    | APIrender


{-| Convert the API names to actual string that is suitable for url
concatenation
-}
toString : PollenRockAPI -> String
toString api =
    case api of
        APIfs ->
            "fs"

        APItags ->
            "tags"

        APIconfig ->
            "config"

        APIwatch ->
            "watch"

        APIrender ->
            "render"


serverUrl : String
serverUrl =
    ""


apiUrl : String
apiUrl =
    serverUrl ++ "/rest"


{-| Encode a list of key-value pair to
application/x-www-form-urlencoded form
-}
encodeParams : List ( String, String ) -> String
encodeParams kvList =
    let
        encodeParam ( k, v ) =
            Http.encodeUri k ++ "=" ++ Http.encodeUri v
    in
        String.join "&"
            (List.map encodeParam kvList)


{-| Do HTTP POST to `whichAPI` with `body`, with request on
`resource`. The return JSON will be decoded using `decoder`.

Note that body will be encoded in x-www-form-urlencoded

-}
post :
    PollenRockAPI
    -> Json.Decoder a
    -> List ( String, String )
    -> String
    -> Http.Request a
post whichAPI decoder body resource =
    Http.post (Util.concatUrl [ apiUrl, toString whichAPI, resource ])
        (Http.stringBody
            "application/x-www-form-urlencoded"
            (encodeParams body)
        )
        decoder


{-| Do HTTP GET to `whichAPI`, with request on `resource`. The
return JSON will be decoded using `decoder`.
-}
get : PollenRockAPI -> Json.Decoder a -> String -> Http.Request a
get whichAPI decoder resource =
    Http.get (Util.concatUrl [ apiUrl, toString whichAPI, resource ]) decoder
