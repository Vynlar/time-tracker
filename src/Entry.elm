module Entry exposing
    ( Entry
    , getId
    , getText
    , getTime
    , isIn
    , newIn
    , newOut
    )

{-| An `Entry` is a single time log containing a unique id, a time, and in the case of `In` a message as well.
-}

import BasicTime exposing (Time)


type Entry
    = In Int Time String
    | Out Int Time


{-| Converts an entry into a string. Because `Out` does not have a message, this function returns a `Maybe String`

    getText (In 42 time "My message") == "My message"

-}
getText : Entry -> Maybe String
getText entry =
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


getId : Entry -> Int
getId entry =
    case entry of
        In id _ _ ->
            id

        Out id _ ->
            id


newIn : Int -> Time -> String -> Entry
newIn id time text =
    In id time text


newOut : Int -> Time -> Entry
newOut id time =
    Out id time


isIn : Entry -> Bool
isIn entry =
    case entry of
        In _ _ _ ->
            True

        Out _ _ ->
            False
