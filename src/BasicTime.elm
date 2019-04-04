module BasicTime exposing
    ( Time
    , am
    , duration
    , new
    , parser
    , pm
    , posixToTime
    , toString
    , toTotalMinutes
    , unixToTime
    )

import Parser exposing ((|.), (|=), Parser)
import Time


type Time
    = Time Int


toTotalMinutes : Time -> Int
toTotalMinutes time =
    case time of
        Time minutes ->
            minutes


toTotalHours : Time -> Int
toTotalHours time =
    case time of
        Time minutes ->
            minutes // 60


toHours : Time -> Int
toHours time =
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


toMinutes : Time -> Int
toMinutes time =
    case time of
        Time minutes ->
            modBy 60 minutes


type Segment
    = AM
    | PM


am : Segment
am =
    AM


pm : Segment
pm =
    PM


toSegment : Time -> Segment
toSegment time =
    let
        hours =
            time
                |> toTotalHours
    in
    if hours < 12 then
        AM

    else
        PM


new : Int -> Int -> Segment -> Time
new hours minutes segment =
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


segmentToString : Segment -> String
segmentToString segment =
    case segment of
        AM ->
            "AM"

        PM ->
            "PM"


toString : Time -> String
toString time =
    String.fromInt (time |> toHours)
        ++ ":"
        ++ (time
                |> toMinutes
                |> String.fromInt
                |> String.padLeft 2 '0'
           )
        ++ " "
        ++ (time
                |> toSegment
                |> segmentToString
           )


duration : Time -> Time -> Time
duration a b =
    case ( a, b ) of
        ( Time x, Time y ) ->
            y - x |> abs |> Time



{-
   Input Parsers
-}


parseSegment : Parser Segment
parseSegment =
    Parser.oneOf
        [ Parser.map (\_ -> AM) (Parser.keyword "AM")
        , Parser.map (\_ -> PM) (Parser.keyword "PM")
        , Parser.map (\_ -> AM) (Parser.keyword "A")
        , Parser.map (\_ -> PM) (Parser.keyword "P")
        , Parser.map (\_ -> AM) (Parser.succeed ())
        ]


parser : Parser Time
parser =
    Parser.succeed new
        |. Parser.spaces
        |= timeIntParser
        |. Parser.symbol ":"
        |= timeIntParser
        |. Parser.spaces
        |= parseSegment


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


posixToTime : Time.Zone -> Time.Posix -> Time
posixToTime zone posix =
    let
        hours =
            Time.toHour zone posix

        minutes =
            Time.toMinute zone posix
    in
    new hours minutes am


unixToTime : Time.Zone -> Int -> Time
unixToTime zone unix =
    Time.millisToPosix (unix * 1000)
        |> posixToTime zone
