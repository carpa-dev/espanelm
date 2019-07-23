module Game.GameCommon exposing (Conjugation(..), GameSettings, Person(..), PersonsRec, Verb, allConjugations, allPersons, conjugationExample, conjugationToString, personToSpanish, personToString)


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
            "Ella"

        FirstPlural ->
            "Nosotros"

        SecondPlural ->
            "Vosotros"

        ThirdPlural ->
            "Ustedes"


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


conjugationExample : Conjugation -> String
conjugationExample c =
    case c of
        ConditionalPerfect ->
            "Yo habría comido"

        ConditionalPresent ->
            "Yo comería"

        ImperativeAffirmativePresent ->
            "Come la comida"

        ImperativeNegativePresent ->
            "No comas la comida"

        IndicativeFuture ->
            "Yo comeré"

        IndicativeFuturePerfect ->
            "Yo habré comido"

        IndicativeImperfect ->
            "Yo comía"

        IndicativePastPerfect ->
            "Yo había comido"

        IndicativePresent ->
            "Yo como"

        IndicativePresentPerfect ->
            "Yo he comido"

        IndicativePresentProgressive ->
            "Estoy comiendo"

        IndicativePreterite ->
            "Yo comí"

        SubjunctivePresent ->
            "Yo coma"


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
    [ IndicativePresent
    , IndicativePreterite
    , IndicativeFuture
    , ConditionalPresent
    , ConditionalPerfect
    , IndicativePresentPerfect
    , IndicativePresentProgressive
    , ImperativeAffirmativePresent
    , ImperativeNegativePresent
    , IndicativeFuturePerfect
    , IndicativeImperfect
    , IndicativePastPerfect
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
