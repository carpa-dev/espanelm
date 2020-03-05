module Translate.Article exposing (Article, Articles, decode, decodeList, individualArticleDec, sortByDateDesc)

import Date exposing (format, fromPosix)
import Json.Decode as Decode exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Time exposing (millisToPosix, utc)


type alias Article =
    { lang : String
    , title : String
    , body : List String
    }


type alias Articles =
    { publishedAt : Time.Posix
    , image : String
    , ptbr : Article
    , es : Article
    }


type PosixSeconds
    = PosixSeconds Int



-- Decoder


decodeList : Decoder (List Articles)
decodeList =
    Decode.list decode


decode : Decoder Articles
decode =
    Decode.succeed Articles
        |> required "publishedAt" posixSecondsDecoder
        |> optional "image" string "/placeholder.png"
        |> required "ptbr"
            individualArticleDec
        |> required "es"
            individualArticleDec



-- TODO: sanity check on the actual int value


posixSecondsDecoder : Decoder Time.Posix
posixSecondsDecoder =
    Decode.map (\a -> a |> posixConstructor |> posixSecondsToMs |> millisToPosix) int


posixConstructor : Int -> PosixSeconds
posixConstructor a =
    PosixSeconds a


posixSecondsToMs : PosixSeconds -> Int
posixSecondsToMs (PosixSeconds s) =
    s * 1000


sortByDateDesc : Articles -> Articles -> Order
sortByDateDesc a b =
    compare (Time.toMillis Time.utc b.publishedAt) (Time.toMillis Time.utc a.publishedAt)


posixSecondsToInt : PosixSeconds -> Int
posixSecondsToInt (PosixSeconds s) =
    s


individualArticleDec : Decoder Article
individualArticleDec =
    Decode.succeed Article
        |> required "lang" string
        |> required "title" string
        |> required "body" (list string)
