module GameCommon exposing (Conjugation, Person(..), allConjugations, allPersons, conjugationToString, personToString)


type Person
    = FirstSingular
    | SecondSingular
    | ThirdSingular
    | FirstPlural
    | SecondPlural
    | ThirdPlural


personToString : Person -> String
personToString person =
    case person of
        FirstSingular ->
            "First Singular"

        SecondSingular ->
            "Second singular"

        ThirdSingular ->
            "Third Singular"

        FirstPlural ->
            "First Plural"

        SecondPlural ->
            "Second Plural"

        ThirdPlural ->
            "Third Plural"


type Conjugation
    = ConditionalPerfect
    | ConditionalPresent
    | ImperativeAffirmativePresent
    | ImperativeNegativePresent
    | IndicativeFuture
    | IndicativeFuturePerfect
    | IndicativeImperfect
    | IndicativePastPerfect
    | IndicativePresent
    | IndicativePresentPerfect
    | IndicativePresentProgressive
    | IndicativePreterite
    | SubjunctivePresent


conjugationToString : Conjugation -> String
conjugationToString conjugation =
    case conjugation of
        ConditionalPerfect ->
            "Conditional Perfect"

        ConditionalPresent ->
            "Conditional Present"

        ImperativeAffirmativePresent ->
            "Imperative Affirmative Present"

        ImperativeNegativePresent ->
            "Imperative Negative Present"

        IndicativeFuture ->
            "Indicative Future"

        IndicativeFuturePerfect ->
            "Indicative Future Perfect"

        IndicativeImperfect ->
            "Indicative Imperfect"

        IndicativePastPerfect ->
            "Indicative Past Perfect"

        IndicativePresent ->
            "Indicative Present"

        IndicativePresentPerfect ->
            "Indicative Present Perfect"

        IndicativePresentProgressive ->
            "Indicative Present Progressive"

        IndicativePreterite ->
            "Indicative Preterite"

        SubjunctivePresent ->
            "Subjunctive Present"


allConjugations : List Conjugation
allConjugations =
    [ ConditionalPerfect
    , ConditionalPresent
    , ImperativeAffirmativePresent
    , ImperativeNegativePresent
    , IndicativeFuture
    , IndicativeFuturePerfect
    , IndicativeImperfect
    , IndicativePastPerfect
    , IndicativePresent
    , IndicativePresentPerfect
    , IndicativePresentProgressive
    , IndicativePreterite
    , SubjunctivePresent
    ]


allPersons : List Person
allPersons =
    [ FirstSingular
    , SecondSingular
    , ThirdSingular
    , FirstPlural
    , SecondPlural
    , ThirdPlural
    ]
