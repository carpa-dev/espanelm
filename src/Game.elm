module Game exposing (Model, Msg(..), init, update, view)

import Game.Round exposing (Round)
import GameCommon exposing (Conjugation(..), Person(..), Verb)
import Html exposing (Html, a, button, div, form, h1, h3, h4, img, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)


type alias Model =
    { rounds : List Round
    , answer : String
    , verbs : List Verb
    , loadedAnswers : List Answer
    , verbsToBeLoaded : List Verb
    , conjugations : List Conjugation
    }


initialModel : Model
initialModel =
    { rounds = []
    , answer = ""
    , verbs = []
    , verbsToBeLoaded = []
    , loadedAnswers = []
    , conjugations = []
    }


type Msg
    = UpdateAnswer String
    | StopGame
    | GotAnswerData (Result Http.Error Answer)
    | VerifyUserAnswer


init : List Verb -> List GameCommon.Conjugation -> ( Model, Cmd Msg )
init verbs conjugations =
    ( { initialModel | verbs = verbs, conjugations = conjugations }, loadResponses verbs )


type alias Answer =
    { verb : Verb
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


type alias PersonsRec =
    { firstSingular : Verb
    , secondSingular : Verb
    , thirdSingular : Verb
    , firstPlural : Verb
    , secondPlural : Verb
    , thirdPlural : Verb
    }


answerDecoder : Decoder Answer
answerDecoder =
    Decode.succeed Answer
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


loadResponses : List Verb -> Cmd Msg
loadResponses verbs =
    Cmd.batch (List.map (\verb -> Http.get { url = "./verbs/" ++ verb ++ ".json", expect = Http.expectJson GotAnswerData answerDecoder }) verbs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        UpdateAnswer answer ->
            ( { model | answer = answer }, Cmd.none )

        -- this should be handled by the father
        StopGame ->
            ( model, Cmd.none )

        GotAnswerData (Ok data) ->
            let
                verbsLeftToBeLoaded =
                    List.filter (\v -> data.verb /= v) model.verbsToBeLoaded

                newModel =
                    { model | verbsToBeLoaded = verbsLeftToBeLoaded, loadedAnswers = data :: model.loadedAnswers }
            in
            case List.length newModel.verbsToBeLoaded of
                0 ->
                    -- generate rounds
                    ( { newModel | rounds = generateRounds newModel.verbs newModel.conjugations newModel.loadedAnswers }, Cmd.none )

                -- still needs more verbs to be loaded
                _ ->
                    ( newModel, Cmd.none )

        GotAnswerData (Err _) ->
            let
                _ =
                    Debug.log "failed"
            in
            ( model, Cmd.none )

        VerifyUserAnswer ->
            let
                answer =
                    model.answer

                round =
                    currentRound model
            in
            case round of
                Just r ->
                    if answer == r.answer then
                        ( { model | rounds = Maybe.withDefault [] (List.tail model.rounds), answer = "" }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


findAnswerFor : Verb -> GameCommon.Conjugation -> Person -> List Answer -> String
findAnswerFor verb conjugation person answers =
    let
        foundVerb =
            lookupByVerb verb answers
    in
    case foundVerb of
        Just answer ->
            answer
                |> lookupByConjugation conjugation
                |> lookupByPerson person

        -- TODO: handle
        Nothing ->
            ""


lookupByVerb : Verb -> List Answer -> Maybe Answer
lookupByVerb verb answers =
    List.head (List.filter (\v -> v.verb == verb) answers)


lookupByPerson : Person -> PersonsRec -> String
lookupByPerson person personsRec =
    case person of
        FirstSingular ->
            personsRec.firstSingular

        SecondSingular ->
            personsRec.secondSingular

        ThirdSingular ->
            personsRec.thirdSingular

        FirstPlural ->
            personsRec.firstPlural

        SecondPlural ->
            personsRec.secondPlural

        ThirdPlural ->
            personsRec.thirdPlural


lookupByConjugation : GameCommon.Conjugation -> Answer -> PersonsRec
lookupByConjugation conjugation answer =
    case conjugation of
        ConditionalPerfect ->
            answer.conditionalPerfect

        ConditionalPresent ->
            answer.conditionalPresent

        ImperativeAffirmativePresent ->
            answer.imperativeAffirmativePresent

        ImperativeNegativePresent ->
            answer.imperativeNegativePresent

        IndicativeFuture ->
            answer.indicativeFuture

        IndicativeFuturePerfect ->
            answer.indicativeFuturePerfect

        IndicativeImperfect ->
            answer.indicativeImperfect

        IndicativePastPerfect ->
            answer.indicativePastPerfect

        IndicativePresent ->
            answer.indicativePresent

        IndicativePresentPerfect ->
            answer.indicativePresentPerfect

        IndicativePresentProgressive ->
            answer.indicativePresentProgressive

        IndicativePreterite ->
            answer.indicativePreterite

        SubjunctivePresent ->
            answer.subjunctivePresent


roundWithConjugationAndVerbAndPerson : Conjugation -> Verb -> Person -> List Answer -> Round
roundWithConjugationAndVerbAndPerson conjugation verb person answers =
    { person = person, verb = verb, conjugation = conjugation, answer = findAnswerFor verb conjugation person answers }


generateRoundWithTenseAndVerb : Conjugation -> Verb -> List Answer -> List Round
generateRoundWithTenseAndVerb conjugation verb answers =
    List.map (\person -> roundWithConjugationAndVerbAndPerson conjugation verb person answers) GameCommon.allPersons


generateRoundWithVerb : List Conjugation -> Verb -> List Answer -> List Round
generateRoundWithVerb conjugations verb answers =
    List.concat (List.map (\c -> generateRoundWithTenseAndVerb c verb answers) conjugations)


generateRounds : List Verb -> List Conjugation -> List Answer -> List Round
generateRounds verbs conjugations answers =
    -- todo: suffle based on game strategy
    List.concat (List.map (\v -> generateRoundWithVerb conjugations v answers) verbs)



-- view


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick StopGame ]
            [ text "back" ]
        , viewCard model
        , viewAllRounds model
        ]


viewAllRounds : Model -> Html Msg
viewAllRounds model =
    div [] (List.map (\round -> div [] [ text ("( " ++ round.verb ++ ", " ++ GameCommon.conjugationToString round.conjugation ++ ", " ++ GameCommon.personToString round.person ++ ", " ++ round.answer ++ " ) ") ]) model.rounds)


viewCard : Model -> Html Msg
viewCard model =
    let
        round =
            currentRound model
    in
    case round of
        Just r ->
            div [ class "card" ]
                [ h3 []
                    [ text <| r.verb ++ " (" ++ GameCommon.conjugationToString r.conjugation ++ ")" ]
                , form [ onSubmit VerifyUserAnswer ]
                    [ span
                        []
                        [ text <| GameCommon.personToSpanish r.person ]
                    , input [ onInput UpdateAnswer, placeholder "type the conjugation", value model.answer ]
                        []
                    , button
                        []
                        [ text "confirm" ]
                    ]
                ]

        Nothing ->
            div [] [ text "muy bien! game is finished" ]



-- helpers


currentRound : Model -> Maybe Round
currentRound model =
    List.head model.rounds
