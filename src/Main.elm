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
        , maximum
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


type Entry
    = In Time Issue
    | Out Time


getIssue : Entry -> Maybe Issue
getIssue entry =
    case entry of
        In _ issue ->
            Just issue

        Out _ ->
            Nothing


getTime : Entry -> Time
getTime entry =
    case entry of
        In time _ ->
            time

        Out time ->
            time


type alias Log =
    List Entry


type alias Model =
    { log : Log
    , timeZone : Time.Zone
    , issue : Issue
    , timeInput : String
    , width : Int
    , height : Int
    }


type alias Flags =
    {}


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
            [ In (makeTime 9 0 AM) "GBC-10"
            , In (makeTime 9 15 AM) "GBC-457"
            , Out (makeTime 12 5 PM)
            , In (makeTime 1 0 PM) "GBC-457"
            , In (makeTime 1 45 PM) "GBC-342"
            , In (makeTime 5 45 PM) "MD-120"
            , Out (makeTime 6 5 PM)
            ]
      , timeZone = Time.utc
      , issue = ""
      , timeInput = ""
      , width = 1280
      , height = 720
      }
    , Cmd.batch
        [ Task.perform SetTimezone Time.here
        , Task.perform (\viewport -> WindowResize (round viewport.viewport.width) (round viewport.viewport.height)) Browser.Dom.getViewport
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


sortLog : Log -> Log
sortLog log =
    List.sortBy (timeToTotalMinutes << getTime) log


parseTime : Parser Time
parseTime =
    Parser.succeed makeTime
        |= Parser.int
        |. Parser.symbol ":"
        |= Parser.int
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.map (\_ -> AM) (Parser.keyword "AM")
            , Parser.map (\_ -> PM) (Parser.keyword "PM")
            ]


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
                | log = In time issue :: model.log
                , timeInput = ""
                , issue = ""
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
                ( Tuple.first x, Tuple.second x + Tuple.second y ) :: removeDuplicates xs

            else
                x :: y :: removeDuplicates xs


calculateTotals : Log -> List ( String, Int )
calculateTotals log =
    List.map2 (\x y -> ( x, y )) log (List.tail log |> Maybe.withDefault [])
        |> List.filter
            (\( first, _ ) ->
                case first of
                    In _ _ ->
                        True

                    Out _ ->
                        False
            )
        |> List.map
            (\( first, second ) ->
                let
                    firstTime =
                        getTime first

                    issue =
                        getIssue first
                            |> Maybe.withDefault ""

                    secondTime =
                        getTime second

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
                | log = In time issue :: model.log
                , issue = ""
              }
            , Cmd.none
            )

        CreateEntryWithIssue issue ->
            createEntry issue model

        NowPressed ->
            ( model, Task.perform (\posix -> ChangeTimeInput ((timeToString << posixToTime model.timeZone) posix)) Time.now )


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
    case getIssue entry of
        Nothing ->
            row
                [ paddingXY 15 10
                , Font.size 16
                , width fill
                ]
                [ el
                    [ alignRight
                    , Font.color (rgba 0 0 0 0.5)
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
                ]
                [ text (Maybe.withDefault "Out" <| getIssue entry)
                , el
                    [ alignRight
                    , Font.color (rgba 0 0 0 0.5)
                    ]
                    (text <| timeToString (getTime entry))
                ]


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
            { onPress = Just (CreateEntryWithIssue issue)
            , label = text issue
            }
        )


renderTitle : String -> Element Msg
renderTitle title =
    el [ Font.size 20, Font.heavy ] (text title)


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
                    [ renderTitle "Log"
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
                    ([ renderTitle "Entries" ]
                        ++ (model.log
                                |> sortLog
                                |> List.map (renderEntry model.timeZone)
                           )
                    )
                , column [ alignTop, width (fillPortion 1), spacing 12 ]
                    ([ renderTitle "Summary" ]
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
                                    (calculateTotals model.log)
                                )
                           ]
                    )
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResize
