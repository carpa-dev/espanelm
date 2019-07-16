module Game.Round exposing (Round, generateRounds)

import Game.GameCommon as GameCommon exposing (Conjugation(..), GameSettings, Person(..), PersonsRec, Verb)
import Game.VerbData exposing (VerbData)


type alias Round =
    { person : Person, verb : Verb, conjugation : Conjugation, answer : String }


generateRounds : GameSettings -> List VerbData -> List Round
generateRounds gameSettings verbData =
    -- todo: suffle based on game strategy
    List.concat (List.map (\v -> generateRoundWithVerb gameSettings.conjugations v verbData) gameSettings.verbs)



-- helpers


roundWithConjugationAndVerbAndPerson : Conjugation -> Verb -> Person -> List VerbData -> Round
roundWithConjugationAndVerbAndPerson conjugation verb person answers =
    { person = person, verb = verb, conjugation = conjugation, answer = findAnswerFor verb conjugation person answers }


generateRoundWithTenseAndVerb : Conjugation -> Verb -> List VerbData -> List Round
generateRoundWithTenseAndVerb conjugation verb answers =
    List.map (\person -> roundWithConjugationAndVerbAndPerson conjugation verb person answers) GameCommon.allPersons


generateRoundWithVerb : List Conjugation -> Verb -> List VerbData -> List Round
generateRoundWithVerb conjugations verb answers =
    List.concat (List.map (\c -> generateRoundWithTenseAndVerb c verb answers) conjugations)


findAnswerFor : Verb -> GameCommon.Conjugation -> Person -> List VerbData -> String
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


lookupByVerb : Verb -> List VerbData -> Maybe VerbData
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


lookupByConjugation : GameCommon.Conjugation -> VerbData -> PersonsRec
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
