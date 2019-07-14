module Game exposing (Model, Msg(..), Round, Verb, init, update, view)

import GameCommon
import Html exposing (Html, a, button, div, h1, h3, h4, img, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)


type alias Verb =
    String


type alias Round =
    { person : GameCommon.Person, verb : Verb, conjugation : GameCommon.Conjugation }


type alias Model =
    { rounds : List Round
    , answer : String
    }



-- TODO maybe rounds could be populated since that's the first thing we run


initialModel : Model
initialModel =
    { rounds = []
    , answer = ""
    }


type Msg
    = UpdateAnswer String
    | StopGame


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAnswer answer ->
            ( { model | answer = answer }, Cmd.none )

        -- this should be handled by the father
        StopGame ->
            ( model, Cmd.none )


generateRoundWithTenseAndVerb : GameCommon.Conjugation -> Verb -> List Round
generateRoundWithTenseAndVerb conjugation verb =
    List.map (\p -> { person = p, verb = verb, conjugation = conjugation }) GameCommon.allPersons


generateRoundWithVerb : List GameCommon.Conjugation -> Verb -> List Round
generateRoundWithVerb conjugations verb =
    List.concat (List.map (\c -> generateRoundWithTenseAndVerb c verb) conjugations)


generateRounds : List Verb -> List GameCommon.Conjugation -> List Round
generateRounds verbs conjugations =
    -- todo: suffle based on game strategy
    List.concat (List.map (\v -> generateRoundWithVerb conjugations v) verbs)



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
    div [] (List.map (\round -> div [] [ text ("( " ++ round.verb ++ ", " ++ GameCommon.conjugationToString round.conjugation ++ ", " ++ GameCommon.personToString round.person ++ " ) ") ]) model.rounds)


viewCard : Model -> Html Msg
viewCard model =
    div [ class "card" ]
        [ h3 [] [ text "i should be a verb" ]
        , input [ onInput UpdateAnswer, placeholder "type the conjugation" ] []
        , button [] [ text "confirm" ]
        ]



-- TODO this method shouldn't even be necessary


stringToVerbs : String -> List Verb
stringToVerbs s =
    s
        |> String.split ","
        |> List.map String.trim
