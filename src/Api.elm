module Api exposing (RemoteStatus, getStatuses, getToken, remoteStatusToEntry)

import BasicTime exposing (Time)
import Entry exposing (Entry)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time


type StatusState
    = In
    | Out


type alias RemoteStatus =
    { unixTime : Int
    , message : String
    , status : StatusState
    }


remoteStatusToEntry : Time.Zone -> Int -> RemoteStatus -> Entry
remoteStatusToEntry zone id { unixTime, message, status } =
    let
        time =
            BasicTime.unixToTime zone unixTime
    in
    case status of
        In ->
            Entry.newIn id time message

        Out ->
            Entry.newOut id time


decodeStatusState : String -> Decoder StatusState
decodeStatusState stateString =
    case stateString of
        "out" ->
            Decode.succeed Out

        _ ->
            Decode.succeed In


decodeStatuses : Decoder (List RemoteStatus)
decodeStatuses =
    Decode.field "statuses"
        (Decode.list
            (Decode.map3
                RemoteStatus
                (Decode.field "time" Decode.int)
                (Decode.field "message" (Decode.nullable Decode.string |> Decode.andThen (Decode.succeed << Maybe.withDefault "No issue")))
                (Decode.field "status" Decode.string |> Decode.andThen decodeStatusState)
            )
        )


getStatuses : String -> (Result Http.Error (List RemoteStatus) -> msg) -> Cmd msg
getStatuses token msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = "/api/simpleinout/statuses"
        , expect = Http.expectJson msg decodeStatuses
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
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
