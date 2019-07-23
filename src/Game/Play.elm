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
    , answer : String
    , gameSettings : GameSettings
    }


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
    ( { gameSettings = gameSettings, answer = "", rounds = rounds, roundsLeft = rounds }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        RestartGame ->
            ( { model | answer = "", roundsLeft = model.rounds }, Cmd.none )

        UpdateAnswer answer ->
            ( { model | answer = answer }, Cmd.none )

        -- this should be handled by the father
        StopGame ->
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
                        ( { model | roundsLeft = Maybe.withDefault [] (List.tail model.roundsLeft), answer = "" }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div []
            [ button
                [ onClick StopGame ]
                [ text "back" ]
            , div [ class "columns is-centered" ]
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
            div [ class "card game-card" ]
                [ header [ class "card-header" ]
                    [ div [ class "card-header-title" ]
                        [ p
                            [ class "has-text-centered" ]
                            [ text <| GameCommon.conjugationToString r.conjugation
                            ]
                        ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "content" ]
                        [ h4 [ class "title is-4" ] [ text r.verb ]
                        , form [ onSubmit VerifyUserAnswer ]
                            [ div
                                [ class "field is-horizontal" ]
                                [ span
                                    [ class "field-label is-medium" ]
                                    [ text <| GameCommon.personToSpanish r.person ]
                                , input [ onInput UpdateAnswer, placeholder "type the conjugation", value model.answer, class "input is-medium field-body" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , footer [ class "card-footer" ]
                    [ div [ class "card-footer-item" ]
                        [ button
                            [ class "button is-white", onClick StopGame ]
                            [ text "quit" ]
                        ]
                    , div [ class "card-footer-item" ]
                        [ button
                            [ class "button is-primary " ]
                            [ text "confirm" ]
                        ]
                    ]
                ]

        Nothing ->
            section [ class "message is-success card game-card" ]
                [ div [ class "message-header" ] [ text "Muy bien!" ]
                , div
                    [ class "message-body" ]
                    [ div [ class "columns" ]
                        [ div [ class "column" ]
                            [ text "Do you want to play again or with different verbs/tenses?"
                            , div [ class "column" ]
                                [ button [ class "button", onClick RestartGame ] [ text "Play again" ] ]
                            , div [ class "column" ]
                                [ button [ class "button is-primary", onClick StopGame ] [ text "Play with different settings" ]
                                ]
                            ]
                        ]
                    ]
                ]



-- helpers


currentRound : Model -> Maybe Round
currentRound model =
    List.head model.roundsLeft
