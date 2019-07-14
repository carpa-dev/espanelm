module Play exposing (Model, Msg, init, update, view)

import Debug
import Game exposing (Msg(..))
import GameCommon exposing (Conjugation)
import Html exposing (Html, a, button, div, h1, h3, h4, img, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, map2, string)



-- Model


type alias Verb =
    String


type alias Round =
    { person : GameCommon.Person, verb : Verb, conjugation : GameCommon.Conjugation }


type Page
    = PickingSettings
    | Playing Game.Model


type alias GameState =
    { rounds : List Round
    , answer : String
    }


type VerbOptions
    = Failure
    | Loading
    | Success (List Verb)


type alias Model =
    { verbs : String
    , options : VerbOptions
    , unavailable : List String
    , failureCause : String
    , conjugations : List GameCommon.Conjugation
    , page : Page
    , availableConjugations : List GameCommon.Conjugation
    , availablePersons : List GameCommon.Person
    }


initialModel : Model
initialModel =
    { verbs = "abordar, abortar"
    , options = Loading
    , unavailable = []
    , failureCause = ""
    , conjugations = []
    , page = PickingSettings
    , availableConjugations = GameCommon.allConjugations
    , availablePersons = GameCommon.allPersons
    }


type Msg
    = Change String
    | Blur
    | GotVerbOptions (Result Http.Error (List Verb))
    | Play
    | SelectConjugation GameCommon.Conjugation
    | GameMsg Game.Msg


init : ( Model, Cmd Msg )
init =
    ( initialModel, Http.get { url = "/verbs.json", expect = Http.expectJson GotVerbOptions verbOptionsDecoder } )



-- update


getUnavailableVerbs : Model -> List String
getUnavailableVerbs model =
    model.verbs |> String.split "," |> List.filter (\verb -> not (isVerbAvailable model.options verb))



--startGame : Model -> Model
--startGame model =
--    let
--        rounds =
--            generateRounds model
--    in
--    { model | page = Playing { rounds = rounds, answer = "" } }


updateConjugation : Model -> Conjugation -> Model
updateConjugation model conjugation =
    case List.member conjugation model.conjugations of
        True ->
            let
                newConj =
                    List.filter (\c -> c /= conjugation) model.conjugations
            in
            { model | conjugations = newConj }

        False ->
            { model | conjugations = conjugation :: model.conjugations }


updateAnswer : Model -> String -> Model
updateAnswer model answer =
    case model.page of
        -- in fact this should never happen
        PickingSettings ->
            model

        Playing gameState ->
            let
                p =
                    model.page

                newGameState =
                    { gameState | answer = answer }

                newPage =
                    Playing newGameState
            in
            { model | page = newPage }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GameMsg gameMsg, Playing gameModel ) ->
            case gameMsg of
                -- TODO
                -- https://discourse.elm-lang.org/t/modifying-parent-state-from-child-page-in-an-elm-spa-example-like-architecture/2437/3
                StopGame ->
                    ( { model | page = PickingSettings }, Cmd.none )

                _ ->
                    let
                        ( pageModel, pageCmd ) =
                            Game.update gameMsg gameModel
                    in
                    ( { model | page = Playing pageModel }, Cmd.map GameMsg pageCmd )

        -- in practice this should never happen
        ( GameMsg gameMsg, PickingSettings ) ->
            ( model, Cmd.none )

        ( Change verbs, _ ) ->
            ( { model | verbs = verbs }, Cmd.none )

        ( Blur, _ ) ->
            ( { model | unavailable = getUnavailableVerbs model }, Cmd.none )

        ( Play, _ ) ->
            let
                ( m, cmd ) =
                    Game.init
            in
            ( { model | page = Playing m }, Cmd.none )

        --( startGame model, Cmd.none )
        ( GotVerbOptions result, _ ) ->
            case result of
                Ok response ->
                    ( { model | options = Success response }, Cmd.none )

                Err r ->
                    ( { model
                        | options = Failure
                        , failureCause =
                            fetchErrorToString r
                      }
                    , Cmd.none
                    )

        ( SelectConjugation conjugation, _ ) ->
            ( updateConjugation model conjugation, Cmd.none )


view : Model -> Html Msg
view model =
    case model.page of
        Playing m ->
            Game.view m
                |> Html.map GameMsg

        PickingSettings ->
            viewStart model


viewStart : Model -> Html Msg
viewStart model =
    div []
        [ h1 [] [ text "Choose your verbs" ]
        , input [ placeholder "Verbs", value model.verbs, onBlur Blur, onInput Change, style "border" (getInputBorderStyle model) ] []
        , button [ disabled (not (isReadyToPlay model)), onClick Play ] [ text "Play" ]
        , viewUnavailableVerb model
        , viewConjugationList model

        --, viewVerbsList model
        ]



-- for debugging purposes


viewCheckedCheckbox : Model -> Conjugation -> Bool
viewCheckedCheckbox model conjugation =
    List.member conjugation model.conjugations



-- very barebones since we know what


conjugationToID : Conjugation -> String
conjugationToID conjugation =
    conjugation
        |> GameCommon.conjugationToString
        |> String.replace " " "-"
        |> String.toLower


viewConjugationCheckbox : String -> Model -> Conjugation -> Html Msg
viewConjugationCheckbox t model conjugation =
    li []
        [ input [ type_ "checkbox", id (conjugationToID conjugation), checked (viewCheckedCheckbox model conjugation) ] []
        , label [ for (conjugationToID conjugation), onClick (SelectConjugation conjugation) ] [ text t ]
        ]


viewConjugationSettings : Model -> Conjugation -> Html Msg
viewConjugationSettings model conjugation =
    viewConjugationCheckbox (GameCommon.conjugationToString conjugation) model conjugation


viewConjugationList : Model -> Html Msg
viewConjugationList model =
    ul []
        (List.map
            (viewConjugationSettings
                model
            )
            model.availableConjugations
        )



--ul []
--    [ li [] [ text "first" ]
--    ]


viewVerbsList : Model -> Html Msg
viewVerbsList model =
    case model.options of
        Success options ->
            ul
                [ style "max-width" "200px"
                , style "margin" "16px auto"
                ]
                (List.map viewOptionItem options)

        _ ->
            text ""


viewEmptyVerb : Model -> Html Msg
viewEmptyVerb model =
    if hasEmptyVerb model then
        div [ style "color" "red" ] [ text "There are empty verbs on your list" ]

    else
        text ""


viewUnavailableVerb : Model -> Html Msg
viewUnavailableVerb model =
    if not (List.isEmpty model.unavailable) then
        div [ style "color" "red" ] [ text ("The following verbs are not available: " ++ String.join ", " model.unavailable) ]

    else
        text ""


viewOptionItem : Verb -> Html Msg
viewOptionItem verb =
    li [] [ text verb ]


hasEmptyVerb : Model -> Bool
hasEmptyVerb model =
    model.verbs
        |> String.split ","
        |> List.any isVerbEmpty



-- Helpers


isReadyToPlay : Model -> Bool
isReadyToPlay model =
    isValid model && (List.length model.conjugations > 0)


isValid : Model -> Bool
isValid model =
    (model |> getVerbList |> List.length)
        == (model.verbs |> String.split "," |> List.length)


isVerbEmpty : String -> Bool
isVerbEmpty verb =
    verb |> String.trim |> String.isEmpty


isVerbAvailable : VerbOptions -> String -> Bool
isVerbAvailable verb_options verb =
    case verb_options of
        Success options ->
            options
                |> List.member (String.trim verb)

        _ ->
            False


getVerbList : Model -> List String
getVerbList model =
    model.verbs
        |> String.split ","
        |> List.filter (\verb -> not (isVerbEmpty verb) && isVerbAvailable model.options verb)


getInputBorderStyle : Model -> String
getInputBorderStyle model =
    if String.isEmpty model.verbs || isValid model then
        "1px solid grey"

    else
        "1px solid red"


verbOptionsDecoder : Decoder (List Verb)
verbOptionsDecoder =
    list string


fetchErrorToString : Http.Error -> String
fetchErrorToString error =
    case error of
        Http.BadBody err ->
            err

        _ ->
            "wrong"
