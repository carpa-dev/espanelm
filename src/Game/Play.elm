module Game.Play exposing (Model, Msg(..), init, subscriptions, update, view)

import Animation
import Animation.Messenger
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
    , cardAnimation : Animation.Messenger.State Msg
    , phantomCard : Maybe Round -- represents the last round, used for animation
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
    | AnimateCardOut Animation.Msg
    | RunAnimation -- TODO: remove this
    | StartCardAnimation
    | ShowNewCard


init : GameSettings -> List VerbData -> ( Model, Cmd Msg )
init gameSettings verbData =
    let
        rounds =
            generateRounds gameSettings verbData
    in
    ( { gameSettings = gameSettings, answer = Pristine "", rounds = rounds, roundsLeft = rounds, cardAnimation = phantomCardInitialState, phantomCard = Nothing }, Cmd.none )


phantomCardInitialState : Animation.Messenger.State msg
phantomCardInitialState =
    Animation.style
        phantomCardHidden


phantomCardHidden =
    [ Animation.top (Animation.px 0.0)
    , Animation.opacity 1.0
    ]


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
            -- handles the case where
            -- the user submits before animation finishes
            -- so card state must be set to initial state
            -- before animation can start
            let
                newStyle =
                    Animation.interrupt
                        [ Animation.set
                            phantomCardHidden
                        , Animation.Messenger.send StartCardAnimation
                        ]
                        model.cardAnimation

                newModel =
                    updateOnSubmit model
            in
            ( { newModel
                | cardAnimation = newStyle
              }
            , Cmd.none
            )

        StartCardAnimation ->
            let
                newStyle =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.top (Animation.px -300)
                            , Animation.opacity 0.0
                            ]
                        , Animation.Messenger.send ShowNewCard
                        ]
                        model.cardAnimation
            in
            ( { model
                | cardAnimation = newStyle
              }
            , Cmd.none
            )

        AnimateCardOut aMsg ->
            let
                ( newStyle, cmd ) =
                    Animation.Messenger.update aMsg model.cardAnimation
            in
            ( { model | cardAnimation = newStyle }, cmd )

        ShowNewCard ->
            ( { model | cardAnimation = phantomCardInitialState, phantomCard = Nothing }, Cmd.none )

        RunAnimation ->
            let
                newStyle =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.top (Animation.px -100)
                            , Animation.opacity 0.0
                            ]
                        , Animation.Messenger.send ShowNewCard
                        ]
                        model.cardAnimation
            in
            ( { model
                | cardAnimation = newStyle
              }
            , Cmd.none
            )


updateOnSubmit : Model -> Model
updateOnSubmit model =
    if isAnswerCorrect (answerToString model.answer) model then
        let
            previousRound =
                currentRound model

            roundsLeft =
                Maybe.withDefault [] (List.tail model.roundsLeft)
        in
        { model | roundsLeft = roundsLeft, answer = Pristine "", phantomCard = previousRound }

    else
        { model | answer = Invalid (answerToString model.answer) }



-- view


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "section" ]
            [ div [ class "columns is-centered" ]
                [ div [ class "column" ] [ div [ class "cards-wrapper" ] [ viewCards model ] ]
                ]
            ]
        ]


viewCards : Model -> Html Msg
viewCards model =
    let
        round =
            currentRound model
    in
    case round of
        Just r ->
            div [] [ viewPhantomCard model, viewCurrentRoundCard model ]

        Nothing ->
            -- game is over
            div []
                [ div [ class "pyro" ] [ div [ class "before" ] [], div [ class "after" ] [] ]
                , section [ class "victory message is-success card game-card" ]
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
                                [ button [ class "button", onClick RestartGame ] [ text "Restart Game" ] ]
                            , div [ class "column" ]
                                [ button [ class "button is-primary", onClick StopGame ] [ text "Play with different settings" ]
                                ]
                            ]
                        ]
                    ]
                ]


viewCurrentRoundCard : Model -> Html Msg
viewCurrentRoundCard model =
    let
        round =
            currentRound model
    in
    case round of
        Just r ->
            form [ onSubmit VerifyUserAnswer ] [ viewIndividualCard model r ]

        Nothing ->
            span [] []


viewPhantomCard : Model -> Html Msg
viewPhantomCard model =
    case model.phantomCard of
        Just r ->
            div [ class "phantom" ] [ viewIndividualCard model r ]

        Nothing ->
            span [] []



-- This is used to render both phantom and concrete card
-- therefore the cardAttribute has to be smart
-- I don't feel good about this, but I believe it's the easiest way to keep
-- them in sync (visually)


viewIndividualCard : Model -> Round -> Html Msg
viewIndividualCard model round =
    div (cardAttributes model round)
        [ header [ class "card-header" ]
            [ div [ class "card-header-title" ]
                [ p
                    [ class "has-text-centered is-5 title" ]
                    [ text <| GameCommon.conjugationToString round.conjugation
                    ]
                ]
            ]
        , div [ class "card-content" ]
            [ div [ class "content has-text-centered" ]
                [ h4 [ class "title is-4" ] [ text round.verb ]
                , div []
                    [ div
                        [ class "field is-horizontal" ]
                        [ span
                            [ class "field-label is-medium" ]
                            [ text <| GameCommon.personToSpanish round.person ]
                        , div [ class "field-body" ]
                            [ div [ class "field" ]
                                [ input [ onInput UpdateAnswer, placeholder "conjugation", value (answerToString model.answer), class ("input is-medium field-body " ++ inputClass model) ]
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


inputClass : Model -> String
inputClass model =
    case model.answer of
        Invalid _ ->
            "is-danger"

        _ ->
            ""



--cardStyles : Model -> Round -> List Html.Attributes


cardAttributes model round =
    if isCurrentRound model round then
        case model.answer of
            Invalid _ ->
                [ class ("card game-card " ++ stackedCardClass model ++ " wrong") ]

            _ ->
                [ class ("card game-card " ++ stackedCardClass model) ]

    else
        -- Phantom deck
        [ class "card game-card" ] ++ List.concat [ Animation.render model.cardAnimation ]


isCurrentRound : Model -> Round -> Bool
isCurrentRound model round =
    case currentRound model of
        Nothing ->
            False

        Just r ->
            round == r


stackedCardClass : Model -> String
stackedCardClass model =
    let
        baseClass =
            "stacked"

        roundsLeft =
            List.length model.roundsLeft
    in
    case roundsLeft of
        1 ->
            baseClass ++ "-1"

        2 ->
            baseClass ++ "-2"

        3 ->
            baseClass ++ "-3"

        4 ->
            baseClass ++ "-4"

        5 ->
            baseClass ++ "-5"

        _ ->
            baseClass ++ "-n"



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription AnimateCardOut [ model.cardAnimation ]



-- helpers


currentRound : Model -> Maybe Round
currentRound model =
    List.head model.roundsLeft


nextRound : Model -> Maybe Round
nextRound model =
    List.drop 1 model.roundsLeft
        |> List.head


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
