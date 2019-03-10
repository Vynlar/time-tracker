module Main exposing (main)

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
        , inFront
        , link
        , maximum
        , mouseOver
        , padding
        , paddingXY
        , paragraph
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
import Html exposing (Html, div, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import List
import Maybe
import Parser exposing ((|.), (|=), Parser)
import Set
import Task
import Time
import Tuple


main =
    Browser.document
        { update = update
        , view = view
        , init = init
        , subscriptions = subscriptions
        }


type alias Issue =
    String


type alias Minutes =
    Int


type alias Hours =
    Int


type Time
    = Time Minutes


timeToTotalMinutes : Time -> Minutes
timeToTotalMinutes time =
    case time of
        Time minutes ->
            minutes


timeToTotalHours : Time -> Hours
timeToTotalHours time =
    case time of
        Time minutes ->
            minutes // 60


timeToHours : Time -> Hours
timeToHours time =
    case time of
        Time minutes ->
            let
                hours =
                    modBy 12 <| minutes // 60
            in
            if hours == 0 then
                12

            else
                hours


timeToMinutes : Time -> Hours
timeToMinutes time =
    case time of
        Time minutes ->
            modBy 60 minutes


type Segment
    = AM
    | PM


timeToSegment : Time -> Segment
timeToSegment time =
    let
        hours =
            timeToTotalHours time
    in
    if hours < 12 then
        AM

    else
        PM


makeTime : Hours -> Minutes -> Segment -> Time
makeTime hours minutes segment =
    let
        offset =
            case segment of
                AM ->
                    0

                PM ->
                    12

        realHours =
            if hours == 12 then
                offset

            else
                hours + offset
    in
    Time (realHours * 60 + minutes)


type alias Id =
    Int


type Entry
    = In Id Time Issue
    | Out Id Time


getIssue : Entry -> Maybe Issue
getIssue entry =
    case entry of
        In _ _ issue ->
            Just issue

        Out _ _ ->
            Nothing


getTime : Entry -> Time
getTime entry =
    case entry of
        In _ time _ ->
            time

        Out _ time ->
            time


getId : Entry -> Id
getId entry =
    case entry of
        In id _ _ ->
            id

        Out id _ ->
            id


type alias Log =
    List Entry


type alias Model =
    { log : Log
    , timeZone : Time.Zone
    , issue : Issue
    , timeInput : String
    , width : Int
    , height : Int
    , currentTime : Time.Posix
    , nextId : Int
    , simpleInOutAppId : String
    , simpleInOutSecret : String
    , oAuthCode : String
    }


type alias Flags =
    { simpleInOutAppId : String
    , simpleInOutSecret : String
    , oAuthCode : String
    }


formValid : { timeInput : String, issue : String } -> Bool
formValid { timeInput, issue } =
    let
        timeResult =
            timeInput
                |> Parser.run parseTime

        timeValid =
            case timeResult of
                Err _ ->
                    False

                Ok _ ->
                    True
    in
    timeInput /= "" && issue /= "" && timeValid


init : Flags -> ( Model, Cmd Msg )
init flags =
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
      , simpleInOutSecret = flags.simpleInOutSecret
      , oAuthCode = flags.oAuthCode
      }
    , Cmd.batch
        [ Task.perform SetTimezone Time.here
        , Task.perform (\viewport -> WindowResize (round viewport.viewport.width) (round viewport.viewport.height)) Browser.Dom.getViewport
        , Task.perform CurrentTime Time.now
        ]
    )


type Msg
    = NoOp
    | SetTimezone Time.Zone
    | CreateEntry
    | CreateEntryWithIssue Issue
    | CreateWithTime Issue Time
    | ChangeIssue String
    | WindowResize Int Int
    | ChangeTimeInput String
    | NowPressed
    | CurrentTime Time.Posix
    | DeleteEntry Id


sortLog : Log -> Log
sortLog log =
    List.sortBy (timeToTotalMinutes << getTime) log


parseInt : String -> Parser Int
parseInt string =
    let
        fixedString =
            if string == "" then
                "0"

            else
                string
    in
    case String.toInt fixedString of
        Just int ->
            Parser.succeed int

        Nothing ->
            Parser.problem "Time is not a number"


timeIntParser : Parser Int
timeIntParser =
    Parser.succeed identity
        |. Parser.chompWhile (\c -> c == '0')
        |= (Parser.getChompedString (Parser.chompWhile Char.isDigit)
                |> Parser.andThen parseInt
           )


parseSegment : Parser Segment
parseSegment =
    Parser.oneOf
        [ Parser.map (\_ -> AM) (Parser.keyword "AM")
        , Parser.map (\_ -> PM) (Parser.keyword "PM")
        , Parser.map (\_ -> AM) (Parser.keyword "A")
        , Parser.map (\_ -> PM) (Parser.keyword "P")
        , Parser.map (\_ -> AM) (Parser.succeed ())
        ]


parseTime : Parser Time
parseTime =
    Parser.succeed makeTime
        |. Parser.spaces
        |= timeIntParser
        |. Parser.symbol ":"
        |= timeIntParser
        |. Parser.spaces
        |= parseSegment


createEntry : Issue -> Model -> ( Model, Cmd Msg )
createEntry issue model =
    let
        timeResult =
            model.timeInput
                |> Parser.run parseTime
    in
    case timeResult of
        Ok time ->
            ( { model
                | log = In model.nextId time issue :: model.log
                , timeInput = ""
                , issue = ""
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        Err err ->
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


addTimes : Time -> Time -> Time
addTimes (Time a) (Time b) =
    Time (a + b)


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



-- TODO: add the current time to the end of the log always to determine the duration of the very last issue


calculateTotals : Time.Zone -> Time.Posix -> Log -> List ( String, Int )
calculateTotals zone currentTime log =
    let
        currentTimeOutEntry =
            Out 0 (posixToTime zone currentTime)

        sortedLog =
            sortLog (currentTimeOutEntry :: log)
    in
    List.map2 (\x y -> ( x, y )) sortedLog (List.tail sortedLog |> Maybe.withDefault [])
        |> List.filter
            (\( first, _ ) ->
                case first of
                    In _ _ _ ->
                        True

                    Out _ _ ->
                        False
            )
        |> List.map
            (\( first, second ) ->
                let
                    firstTime =
                        getTime first

                    secondTime =
                        getTime second

                    issue =
                        getIssue first
                            |> Maybe.withDefault ""

                    duration =
                        case ( firstTime, secondTime ) of
                            ( Time a, Time b ) ->
                                b - a
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
    makeTime hours minutes AM


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                | log = In model.nextId time issue :: model.log
                , issue = ""
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        CreateEntryWithIssue issue ->
            createEntry issue model

        NowPressed ->
            ( model, Task.perform (\posix -> ChangeTimeInput ((timeToString << posixToTime model.timeZone) posix)) Time.now )

        CurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        DeleteEntry id ->
            ( { model | log = List.filter (\entry -> getId entry /= id) model.log }, Cmd.none )


segmentToString : Segment -> String
segmentToString segment =
    case segment of
        AM ->
            "AM"

        PM ->
            "PM"


timeToString : Time -> String
timeToString time =
    String.fromInt (timeToHours time)
        ++ ":"
        ++ (timeToMinutes time
                |> String.fromInt
                |> String.padLeft 2 '0'
           )
        ++ " "
        ++ (time
                |> timeToSegment
                |> segmentToString
           )


normalShadow : Float -> Attr decorative Msg
normalShadow size =
    Border.shadow
        { offset = ( 0.0, size )
        , size = 0
        , blur = size
        , color = rgba 0 0 0 0.15
        }


renderEntry : Time.Zone -> Entry -> Element Msg
renderEntry zone entry =
    Input.button [ width fill ]
        { label =
            case getIssue entry of
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
                            (text <| timeToString (getTime entry))
                        ]

                Just issue ->
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
                        [ text (Maybe.withDefault "Out" <| getIssue entry)
                        , el
                            [ alignRight
                            , Font.color (rgba 0 0 0 0.5)
                            , spacing 5
                            ]
                            (text <| timeToString (getTime entry))
                        ]
        , onPress = Just (DeleteEntry (getId entry))
        }


getAllIssues : Log -> List Issue
getAllIssues log =
    log
        |> List.map getIssue
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


lightBlue =
    rgb255 228 241 255


blue =
    rgb255 51 140 229


white =
    rgb255 255 255 255


renderQuickButton : Issue -> Element Msg
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
                        [ renderTitle "Entries" ]
                            ++ (model.log
                                    |> sortLog
                                    |> List.map (renderEntry model.timeZone)
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
                                        (text <| "Now " ++ (timeToString << posixToTime model.timeZone) model.currentTime)
                                    ]
                               ]
                    )
                , column [ alignTop, width (fillPortion 1), spacing 12 ]
                    (if List.length model.log == 0 then
                        []

                     else
                        [ renderTitle "Summary" ]
                            ++ [ column [ width fill, Font.size 16 ]
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
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResize
        , Time.every 1000 CurrentTime
        ]
