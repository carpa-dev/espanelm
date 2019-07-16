module Game.VerbData exposing (VerbData, load)

import Game.GameCommon exposing (PersonsRec, Verb)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias VerbData =
    { verb : String
    , englishInfinitive : String
    , irregular : Bool
    , type_ : String
    , reflexive : Bool
    , common : Bool
    , indicativePresent : PersonsRec
    , indicativePreterite : PersonsRec
    , indicativeFuture : PersonsRec
    , conditionalPresent : PersonsRec
    , indicativeImperfect : PersonsRec
    , indicativePresentProgressive : PersonsRec
    , indicativePresentPerfect : PersonsRec
    , indicativePastPerfect : PersonsRec
    , indicativeFuturePerfect : PersonsRec
    , conditionalPerfect : PersonsRec
    , subjunctivePresent : PersonsRec
    , imperativeAffirmativePresent : PersonsRec
    , imperativeNegativePresent : PersonsRec
    , ending : String
    }



--load : List Verb -> Cmd GotVerbData


load : List Verb -> (Result Http.Error VerbData -> msg) -> Cmd msg
load verbs msg =
    Cmd.batch (List.map (\verb -> Http.get { url = "./verbs/" ++ verb ++ ".json", expect = Http.expectJson msg verbDataDecoder }) verbs)


verbDataDecoder : Decoder VerbData
verbDataDecoder =
    Decode.succeed VerbData
        |> required "verb" Decode.string
        |> required "englishInfinitive" Decode.string
        |> required "irregular" Decode.bool
        |> required "type" Decode.string
        |> required "reflexive" Decode.bool
        |> required "common" Decode.bool
        |> required "IndicativePresent" personDecoder
        |> required "IndicativePreterite" personDecoder
        |> required "IndicativeFuture" personDecoder
        |> required "ConditionalPresent" personDecoder
        |> required "IndicativeImperfect" personDecoder
        |> required "IndicativePresentProgressive" personDecoder
        |> required "IndicativePresentPerfect" personDecoder
        |> required "IndicativePastPerfect" personDecoder
        |> required "IndicativeFuturePerfect" personDecoder
        |> required "ConditionalPerfect" personDecoder
        |> required "SubjunctivePresent" personDecoder
        |> required "ImperativeAffirmativePresent" personDecoder
        |> required "ImperativeNegativePresent" personDecoder
        |> required "ending" Decode.string


personDecoder : Decoder PersonsRec
personDecoder =
    Decode.succeed PersonsRec
        |> required "Yo" Decode.string
        |> required "Tu" Decode.string
        |> required "El" Decode.string
        |> required "Nosotros" Decode.string
        |> required "Vosotros" Decode.string
        |> required "Ellos" Decode.string
