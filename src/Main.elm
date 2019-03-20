module Main exposing (main)

import BasicTime exposing (Time)
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Element
    exposing
        ( Attr
        , DeviceClass(..)
        , Element
        , Orientation(..)
        , alignBottom
        , alignRight
        , alignTop
        , centerX
        , centerY
        , classifyDevice
        , column
        , el
        , fill
        , fillPortion
        , height
        , link
        , maximum
        , mouseOver
        , padding
        , paddingXY
        , px
        , rgb255
        , rgba
        , row
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Entry exposing (Entry)
import Html.Attributes exposing (placeholder)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List
import Maybe
import Parser
import Set
import Task
import Time
import Tuple


main : Program Flags Model Msg
main =
    Browser.document
        { update = update
        , view = view
        , init = init
        , subscriptions = subscriptions
        }


type AuthStatus
    = LoggedOut
    | HasCode String
    | HasToken String


type alias Model =
    { log : List Entry
    , timeZone : Time.Zone
    , issue : String
    , timeInput : String
    , width : Int
    , height : Int
    , currentTime : Time.Posix
    , nextId : Int
    , simpleInOutAppId : String
    , authStatus : AuthStatus
    }


type alias Flags =
    { simpleInOutAppId : String
    , oAuthCode : Maybe String
    , token : Maybe String
    }


formValid : { timeInput : String, issue : String } -> Bool
formValid { timeInput, issue } =
    let
        timeResult =
            timeInput
                |> Parser.run BasicTime.parser

        timeValid =
            case timeResult of
                Err _ ->
                    False

                Ok _ ->
                    True
    in
    timeInput /= "" && issue /= "" && timeValid


request : String -> String -> String -> Http.Expect Msg -> Cmd Msg
request method token path expect =
    Http.request
        { method = method
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://simpleinout.com/" ++ path
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


decodeToken : Decoder String
decodeToken =
    Decode.field "token" Decode.string


tokenRequest : String -> Cmd Msg
tokenRequest code =
    Http.post
        { url = "/api/simpleinout/oauth/token"
        , expect = Http.expectJson TokenSuccess decodeToken
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "code", Encode.string code )
                    ]
                )
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        tokenCmd =
            case flags.oAuthCode of
                Nothing ->
                    []

                Just code ->
                    [ tokenRequest code ]

        authStatus =
            case ( flags.token, flags.oAuthCode ) of
                ( Just token, _ ) ->
                    HasToken token

                ( Nothing, Just code ) ->
                    HasCode code

                _ ->
                    LoggedOut
    in
    ( { log =
            []

      {-
         [ In (makeTime 9 0 AM) "GBC-457"
         , In (makeTime 9 15 AM) "GBC-457"
         , In (makeTime 9 10 AM) "GBC-456"
         , In (makeTime 10 15 AM) "GBC-457"
         , Out (makeTime 12 15 PM)
         ]
      -}
      , timeZone = Time.utc
      , issue = ""
      , timeInput = ""
      , width = 1280
      , height = 720
      , currentTime = Time.millisToPosix 0
      , nextId = 0
      , simpleInOutAppId = flags.simpleInOutAppId
      , authStatus = authStatus
      }
    , Cmd.batch
        ([ Task.perform SetTimezone Time.here
         , Task.perform (\viewport -> WindowResize (round viewport.viewport.width) (round viewport.viewport.height)) Browser.Dom.getViewport
         , Task.perform CurrentTime Time.now
         ]
            ++ tokenCmd
        )
    )


type Msg
    = SetTimezone Time.Zone
    | CreateEntry
    | CreateWithTime String Time
    | ChangeIssue String
    | WindowResize Int Int
    | ChangeTimeInput String
    | NowPressed
    | CurrentTime Time.Posix
    | DeleteEntry Int
    | TokenSuccess (Result Http.Error String)


sortLog : List Entry -> List Entry
sortLog log =
    List.sortBy (BasicTime.toTotalMinutes << Entry.getTime) log


createEntry : String -> Model -> ( Model, Cmd Msg )
createEntry issue model =
    let
        timeResult =
            model.timeInput
                |> Parser.run BasicTime.parser
    in
    case timeResult of
        Ok time ->
            ( { model
                | log = Entry.newIn model.nextId time issue :: model.log
                , timeInput = ""
                , issue = ""
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        Err _ ->
            ( model, Task.perform (CreateWithTime issue << posixToTime model.timeZone) Time.now )


durationToString : Int -> String
durationToString duration =
    let
        hours =
            duration // 60

        minutes =
            modBy 60 duration
    in
    [ ( hours, "h" ), ( minutes, "m" ) ]
        |> List.filter (\( d, _ ) -> d > 0)
        |> List.map (\( d, suffix ) -> String.fromInt d ++ suffix)
        |> String.join " "


removeDuplicates : List ( String, Int ) -> List ( String, Int )
removeDuplicates list =
    case list of
        [] ->
            []

        x :: [] ->
            [ x ]

        x :: y :: xs ->
            if Tuple.first x == Tuple.first y then
                removeDuplicates (( Tuple.first x, Tuple.second x + Tuple.second y ) :: xs)

            else
                x :: removeDuplicates (y :: xs)


calculateTotals : Time.Zone -> Time.Posix -> List Entry -> List ( String, Int )
calculateTotals zone currentTime log =
    let
        currentTimeOutEntry =
            Entry.newOut 0 (posixToTime zone currentTime)

        sortedLog =
            sortLog (currentTimeOutEntry :: log)
    in
    List.map2 (\x y -> ( x, y )) sortedLog (List.tail sortedLog |> Maybe.withDefault [])
        |> List.filter
            (\( first, _ ) ->
                Entry.isIn first
            )
        |> List.map
            (\( first, second ) ->
                let
                    firstTime =
                        Entry.getTime first

                    secondTime =
                        Entry.getTime second

                    issue =
                        Entry.getText first
                            |> Maybe.withDefault ""

                    duration =
                        BasicTime.duration firstTime secondTime
                            |> BasicTime.toTotalMinutes
                in
                ( issue, duration )
            )
        |> List.sortBy Tuple.first
        |> removeDuplicates


posixToTime : Time.Zone -> Time.Posix -> Time
posixToTime zone posix =
    let
        hours =
            Time.toHour zone posix

        minutes =
            Time.toMinute zone posix
    in
    BasicTime.new hours minutes BasicTime.am


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        ChangeIssue newIssue ->
            ( { model | issue = newIssue }, Cmd.none )

        ChangeTimeInput newTime ->
            ( { model | timeInput = String.toUpper newTime }, Cmd.none )

        SetTimezone zone ->
            ( { model | timeZone = zone }, Cmd.none )

        CreateEntry ->
            createEntry model.issue model

        CreateWithTime issue time ->
            ( { model
                | log = Entry.newIn model.nextId time issue :: model.log
                , issue = ""
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        NowPressed ->
            ( model, Task.perform (\posix -> ChangeTimeInput ((BasicTime.toString << posixToTime model.timeZone) posix)) Time.now )

        CurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        DeleteEntry id ->
            ( { model | log = List.filter (\entry -> Entry.getId entry /= id) model.log }, Cmd.none )

        TokenSuccess (Ok token) ->
            ( { model | authStatus = HasToken token }, Cmd.none )

        TokenSuccess (Err _) ->
            ( { model | authStatus = LoggedOut }, Cmd.none )


normalShadow : Float -> Attr decorative Msg
normalShadow size =
    Border.shadow
        { offset = ( 0.0, size )
        , size = 0
        , blur = size
        , color = rgba 0 0 0 0.15
        }


renderEntry : Entry -> Element Msg
renderEntry entry =
    Input.button [ width fill ]
        { label =
            case Entry.getText entry of
                Nothing ->
                    row
                        [ paddingXY 15 10
                        , Font.size 16
                        , width fill
                        , mouseOver
                            [ Font.color (rgb255 200 0 0)
                            ]
                        ]
                        [ el
                            [ alignRight
                            , Font.color (rgba 0 0 0 0.5)
                            , spacing 5
                            ]
                            (text <| BasicTime.toString (Entry.getTime entry))
                        ]

                Just _ ->
                    row
                        [ padding 15
                        , normalShadow 3
                        , Border.rounded 5
                        , Font.size 16
                        , width fill
                        , Background.color (rgba 1 1 1 1)
                        , Border.width 2
                        , Border.color (rgba 0 0 0 0)
                        , mouseOver
                            [ Border.color (rgb255 200 0 0)
                            ]
                        ]
                        [ text (Maybe.withDefault "Out" <| Entry.getText entry)
                        , el
                            [ alignRight
                            , Font.color (rgba 0 0 0 0.5)
                            , spacing 5
                            ]
                            (text <| BasicTime.toString (Entry.getTime entry))
                        ]
        , onPress = Just (DeleteEntry (Entry.getId entry))
        }


getAllIssues : List Entry -> List String
getAllIssues log =
    log
        |> List.map Entry.getText
        |> List.filter
            (\x ->
                case x of
                    Nothing ->
                        False

                    Just _ ->
                        True
            )
        |> List.map (Maybe.withDefault "")
        |> Set.fromList
        |> Set.toList


lightBlue : Element.Color
lightBlue =
    rgb255 228 241 255


blue : Element.Color
blue =
    rgb255 51 140 229


white : Element.Color
white =
    rgb255 255 255 255


renderQuickButton : String -> Element Msg
renderQuickButton issue =
    el [ Background.color blue, Font.color white, Border.rounded 200, Font.size 14 ]
        (Input.button [ padding 8 ]
            { onPress = Just (ChangeIssue issue)
            , label = text issue
            }
        )


renderTitle : String -> Element Msg
renderTitle title =
    el [ Font.size 20, Font.heavy ] (text title)


simpleInOutAuthorizeLink : String -> String
simpleInOutAuthorizeLink clientId =
    "https://simpleinout.com/oauth/authorize?response_type=code&client_id="
        ++ clientId
        ++ "&redirect_uri=https://time.adrianaleixandre.com&scope=write&locale=en"


view : Model -> Document Msg
view model =
    let
        device =
            classifyDevice { width = model.width, height = model.height }

        narrow =
            case ( device.class, device.orientation ) of
                ( Phone, _ ) ->
                    True

                ( Tablet, Portrait ) ->
                    True

                ( Tablet, Landscape ) ->
                    False

                ( Desktop, _ ) ->
                    False

                ( BigDesktop, _ ) ->
                    False

        container =
            if narrow then
                column

            else
                row

        valid =
            formValid { timeInput = model.timeInput, issue = model.issue }
    in
    { title = "Timetracker"
    , body =
        [ Element.layout [ padding 30, Background.color (rgba 0 0 0 0.1) ] <|
            container
                [ spacing 30
                , centerX
                , centerY
                , width
                    (fill
                        |> maximum
                            (if narrow then
                                500

                             else
                                1000
                            )
                    )
                ]
                [ column [ alignTop, width (fillPortion 1), spacing 20 ]
                    [ renderTitle "Log time"
                    , link []
                        { url = simpleInOutAuthorizeLink model.simpleInOutAppId
                        , label = text "Log in"
                        }
                    , row [ width fill ]
                        [ Input.text
                            [ width fill
                            , height (px 40)
                            , Font.size 16
                            , Border.roundEach
                                { topLeft = 5
                                , topRight = 0
                                , bottomLeft = 5
                                , bottomRight = 0
                                }
                            , Border.width 0
                            , normalShadow 3
                            , spacing 5
                            ]
                            { onChange = ChangeTimeInput
                            , text = model.timeInput
                            , placeholder = Just (Input.placeholder [] (text "hh:mm"))
                            , label =
                                Input.labelAbove [ Font.size 16 ] (text "Start time")
                            }
                        , Input.button
                            [ Background.color lightBlue
                            , height (px 40)
                            , alignBottom
                            , width (px 70)
                            , Border.roundEach
                                { topLeft = 0
                                , topRight = 5
                                , bottomLeft = 0
                                , bottomRight = 5
                                }
                            , normalShadow 3
                            ]
                            { onPress = Just NowPressed, label = el [ centerX, Font.color blue, Font.size 16 ] (text "Now") }
                        ]
                    , column [ spacing 8, width fill ]
                        [ Input.text
                            [ width fill
                            , height (px 40)
                            , spacing 5
                            , Font.size 16
                            , Border.rounded 5
                            , Border.width 0
                            , normalShadow 3
                            ]
                            { onChange = ChangeIssue
                            , text = model.issue
                            , placeholder = Just (Input.placeholder [] (text "ex. GBC-10, standup"))
                            , label = Input.labelAbove [ Font.size 16 ] (text "Description")
                            }
                        , wrappedRow [ spacing 4 ]
                            (model.log
                                |> getAllIssues
                                |> List.map renderQuickButton
                            )
                        ]
                    , Input.button
                        [ paddingXY 16 12
                        , Background.color
                            (if valid then
                                blue

                             else
                                rgb255 200 200 200
                            )
                        , Font.size 16
                        , Font.color
                            (if valid then
                                white

                             else
                                rgb255 150 150 150
                            )
                        , Border.rounded 5
                        ]
                        { onPress =
                            if valid then
                                Just CreateEntry

                            else
                                Nothing
                        , label = text "Create"
                        }
                    ]
                , column [ alignTop, spacing 10, width (fillPortion 1) ]
                    (if List.length model.log == 0 then
                        []

                     else
                        renderTitle "Entries"
                            :: (model.log
                                    |> sortLog
                                    |> List.map renderEntry
                               )
                            ++ [ row
                                    [ paddingXY 15 10
                                    , Font.size 16
                                    , width fill
                                    ]
                                    [ el
                                        [ alignRight
                                        , Font.color (rgba 0 0 0 0.5)
                                        ]
                                        (text <| "Now " ++ (BasicTime.toString << posixToTime model.timeZone) model.currentTime)
                                    ]
                               ]
                    )
                , column [ alignTop, width (fillPortion 1), spacing 12 ]
                    (if List.length model.log == 0 then
                        []

                     else
                        [ renderTitle "Summary"
                        , column [ width fill, Font.size 16 ]
                            (List.indexedMap
                                (\index ( issue, duration ) ->
                                    row
                                        [ width fill
                                        , paddingXY 8 16
                                        , Border.rounded 3
                                        , Background.color
                                            (if modBy 2 index == 1 then
                                                rgba 0 0 0 0

                                             else
                                                rgb255 240 240 240
                                            )
                                        ]
                                        [ text issue, el [ alignRight ] (text <| durationToString duration) ]
                                )
                                (calculateTotals model.timeZone model.currentTime model.log)
                            )
                        ]
                    )
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize WindowResize
        , Time.every 1000 CurrentTime
        ]
