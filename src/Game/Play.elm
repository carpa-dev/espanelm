module Game.Play exposing (Model, Msg(..), init, update, view)

import Game.GameCommon as GameCommon exposing (Conjugation(..), GameSettings, Person(..), PersonsRec, Verb)
import Game.Round exposing (Round, generateRounds)
import Game.VerbData exposing (VerbData)
import Html exposing (Html, a, button, div, form, h1, h3, h4, img, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)


type alias Model =
    { rounds : List Round
    , answer : String
    , gameSettings : GameSettings
    }


type Msg
    = UpdateAnswer String
    | StopGame
    | VerifyUserAnswer


init : GameSettings -> List VerbData -> ( Model, Cmd Msg )
init gameSettings verbData =
    ( { gameSettings = gameSettings, answer = "", rounds = generateRounds gameSettings verbData }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
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
                        ( { model | rounds = Maybe.withDefault [] (List.tail model.rounds), answer = "" }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



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
