module Api
    exposing
        ( get
        , post
        , PollenRockAPI(..)
        , fsGetResponseDecoder
        , fsPostResponseDecoder
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
