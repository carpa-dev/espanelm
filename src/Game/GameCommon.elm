module Game.GameCommon exposing (Conjugation(..), GameSettings, Person(..), PersonsRec, Verb, allConjugations, allPersons, conjugationToString, personToSpanish, personToString)


type alias Verb =
    String


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
            "First-person singular"

        SecondSingular ->
            "Second-person singular"

        ThirdSingular ->
            "Third-person singular"

        FirstPlural ->
            "First-person plural"

        SecondPlural ->
            "Second-person plural"

        ThirdPlural ->
            "Third-person Plural"


personToSpanish : Person -> String
personToSpanish person =
    case person of
        FirstSingular ->
            "Yo"

        SecondSingular ->
            "Tu"

        ThirdSingular ->
            "El/Ella/Usted"

        FirstPlural ->
            "Nosotros(as)"

        SecondPlural ->
            "Vosotros(as)"

        ThirdPlural ->
            "Nosotros(as)"


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


type alias GameSettings =
    { verbs : List Verb
    , conjugations : List Conjugation

    -- TODO: more settings
    }


type alias PersonsRec =
    { firstSingular : Verb
    , secondSingular : Verb
    , thirdSingular : Verb
    , firstPlural : Verb
    , secondPlural : Verb
    , thirdPlural : Verb
    }
