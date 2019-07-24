module Game.Play exposing (Model, Msg(..), init, update, view)

import Game.GameCommon as GameCommon exposing (Conjugation(..), GameSettings, Person(..), PersonsRec, Verb)
import Game.Round exposing (Round, generateRounds)
import Game.VerbData exposing (VerbData)
import Html exposing (Html, a, button, div, footer, form, h1, h3, h4, header, img, input, label, li, p, section, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)


type alias Model =
    { rounds : List Round
    , roundsLeft : List Round
    , answer : UserAnswer
    , gameSettings : GameSettings
    }


type UserAnswer
    = Pristine String
    | Dirty String
    | Invalid String


type Msg
    = UpdateAnswer String
    | StopGame
    | VerifyUserAnswer
    | RestartGame


init : GameSettings -> List VerbData -> ( Model, Cmd Msg )
init gameSettings verbData =
    let
        rounds =
            generateRounds gameSettings verbData
    in
    ( { gameSettings = gameSettings, answer = Pristine "", rounds = rounds, roundsLeft = rounds }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        RestartGame ->
            ( { model | answer = Pristine "", roundsLeft = model.rounds }, Cmd.none )

        UpdateAnswer answer ->
            ( { model | answer = Dirty answer }, Cmd.none )

        -- this should be handled by the parent
        StopGame ->
            ( model, Cmd.none )

        VerifyUserAnswer ->
            updateOnSubmit model


updateOnSubmit : Model -> ( Model, Cmd Msg )
updateOnSubmit model =
    if isAnswerCorrect (answerToString model.answer) model then
        let
            roundsLeft =
                Maybe.withDefault [] (List.tail model.roundsLeft)
        in
        ( { model | roundsLeft = roundsLeft, answer = Pristine "" }, Cmd.none )

    else
        ( { model | answer = Invalid (answerToString model.answer) }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "section" ]
            [ div [ class "columns is-centered" ]
                [ div [ class "column" ] [ viewCard model ]
                ]
            ]
        ]



-- Debug purposes


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
            form [ class "card game-card", onSubmit VerifyUserAnswer ]
                [ header [ class "card-header" ]
                    [ div [ class "card-header-title" ]
                        [ p
                            [ class "has-text-centered is-5 title" ]
                            [ text <| GameCommon.conjugationToString r.conjugation
                            ]
                        ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "content has-text-centered" ]
                        [ h4 [ class "title is-4" ] [ text r.verb ]
                        , div []
                            [ div
                                [ class "field is-horizontal" ]
                                [ span
                                    [ class "field-label is-medium" ]
                                    [ text <| GameCommon.personToSpanish r.person ]
                                , div [ class "field-body" ]
                                    [ div [ class "field" ]
                                        [ input [ onInput UpdateAnswer, placeholder "type the conjugation", value (answerToString model.answer), class ("input is-medium field-body " ++ inputClass model) ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , footer [ class "card-footer" ]
                    [ div [ class "column" ]
                        [ button
                            [ type_ "button", class "button is-white is-fullwidth", onClick StopGame ]
                            [ text "quit" ]
                        ]
                    , div [ class "column" ]
                        [ button
                            [ class "button is-primary is-fullwidth" ]
                            [ text "confirm" ]
                        ]
                    ]
                ]

        Nothing ->
            div []
                [ div [ class "pyro" ] [ div [ class "before" ] [], div [ class "after" ] [] ]
                , section [ class "message is-success card game-card" ]
                    [ div [ class "message-header" ] [ text "Muy bien!" ]
                    , div
                        [ class "message-body" ]
                        [ div [ class "columns" ]
                            [ div [ class "column" ]
                                [ text "Do you want to play again or with different verbs/tenses?"
                                ]
                            ]
                        , div [ class "columns" ]
                            [ div [ class "column" ]
                                [ button [ class "button", onClick RestartGame ] [ text "Play again" ] ]
                            , div [ class "column" ]
                                [ button [ class "button is-primary", onClick StopGame ] [ text "Play with different settings" ]
                                ]
                            ]
                        ]
                    ]
                ]


inputClass : Model -> String
inputClass model =
    case model.answer of
        Invalid _ ->
            "is-danger"

        _ ->
            ""



-- helpers


currentRound : Model -> Maybe Round
currentRound model =
    List.head model.roundsLeft


answerToString : UserAnswer -> String
answerToString ua =
    case ua of
        Pristine s ->
            s

        Dirty s ->
            s

        Invalid s ->
            s


isAnswerCorrect : String -> Model -> Bool
isAnswerCorrect answer model =
    let
        round =
            currentRound model
    in
    case round of
        Just r ->
            if String.toLower answer == String.toLower r.answer then
                True

            else
                False

        Nothing ->
            False
