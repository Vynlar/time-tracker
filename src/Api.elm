module Api exposing (getStatuses, getToken)

import Entry exposing (Entry)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


decodeStatuses : Decoder (List Entry)
decodeStatuses =
    Decode.fail "wow"


getStatuses : String -> (Result Http.Error (List Entry) -> msg) -> Cmd msg
getStatuses token msg =
    Http.get
        { url = "/api/simpleinout/statuses"
        , expect = Http.expectJson msg decodeStatuses
        }


decodeToken : Decoder String
decodeToken =
    Decode.field "token" Decode.string


getToken : String -> (Result Http.Error String -> msg) -> Cmd msg
getToken code msg =
    Http.post
        { url = "/api/simpleinout/oauth/token"
        , expect = Http.expectJson msg decodeToken
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "code", Encode.string code )
                    ]
                )
        }
