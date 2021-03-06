module Game.Game exposing (Model, Msg, init, initCmd, subscriptions, update, view)

import Animation exposing (px)
import Components.Loading exposing (viewLoading)
import Debug
import Game.AvailableVerbs
import Game.GameCommon exposing (Conjugation, GameSettings, Person, Verb)
import Game.PickingSettings as PickingSettings exposing (Msg(..), OutMsg(..))
import Game.Play as Play exposing (Msg(..))
import Game.VerbData as VerbData exposing (VerbData)
import Html exposing (Html, a, button, div, h1, h3, h4, img, input, label, li, progress, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, map2, string)
import Set



-- Model


type alias LoadingAvailableVerbs =
    Maybe String


type Page
    = LoadingAvailableVerbs LoadingAvailableVerbs
    | PickingSettings
    | PreparingGame -- Loading verbs, generating rounds
    | Playing Play.Model -- Actually playing the game


type alias Model =
    { page : Page
    , pickingSettingsModel : PickingSettings.Model
    , gameSettings : GameSettings
    , verbData : List VerbData
    }


initialModel : Model
initialModel =
    let
        ( psModel, _ ) =
            PickingSettings.init []
    in
    { page = LoadingAvailableVerbs Nothing
    , gameSettings = { verbs = [], conjugations = [] }
    , pickingSettingsModel = psModel
    , verbData = []
    }


type Msg
    = GotVerbOptions (Result Http.Error (List Verb))
    | PlayMsg Play.Msg
    | PickingSettingsMsg PickingSettings.Msg
    | GotVerbData (Result Http.Error VerbData)


init : Model
init =
    initialModel


initCmd : Model -> Cmd Msg
initCmd model =
    -- no need to load data when it's already loaded!
    if List.isEmpty model.pickingSettingsModel.availableVerbs then
        Game.AvailableVerbs.load GotVerbOptions

    else
        Cmd.none



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( PlayMsg playMsg, Playing playModel ) ->
            case playMsg of
                StopGame ->
                    ( { model | page = PickingSettings }, Cmd.none )

                _ ->
                    let
                        ( pageModel, pageCmd ) =
                            Play.update playMsg playModel
                    in
                    ( { model | page = Playing pageModel }, Cmd.map PlayMsg pageCmd )

        ( PickingSettingsMsg psMsg, _ ) ->
            let
                ( m, c, parentMsg ) =
                    PickingSettings.update psMsg model.pickingSettingsModel
            in
            case parentMsg of
                Play gameSettings ->
                    ( { model | page = PreparingGame, gameSettings = gameSettings }, VerbData.load gameSettings.verbs GotVerbData )

                _ ->
                    ( { model | page = PickingSettings, pickingSettingsModel = m }, Cmd.map PickingSettingsMsg c )

        ( GotVerbOptions result, _ ) ->
            case result of
                Ok availableVerbs ->
                    let
                        pickingSettingsModel =
                            model.pickingSettingsModel

                        ( m, cmd ) =
                            PickingSettings.init availableVerbs
                    in
                    ( { model | page = PickingSettings, pickingSettingsModel = m }, Cmd.none )

                Err r ->
                    ( { model
                        | page = LoadingAvailableVerbs (Just <| fetchErrorToString r)
                      }
                    , Cmd.none
                    )

        ( GotVerbData r, _ ) ->
            ( updateLoadVerbData model r, Cmd.none )

        ( PlayMsg gmsg, _ ) ->
            ( model, Cmd.none )


updateLoadVerbData : Model -> Result Http.Error VerbData -> Model
updateLoadVerbData model msg =
    -- Pushes loaded verbs to verbData
    -- When it's done, changes page to playing
    case msg of
        Err err ->
            -- TODO
            --Debug.todo "Implement sad path for upload verb data"
            model

        Ok data ->
            let
                newModel =
                    { model | verbData = data :: model.verbData }

                verbsLeft =
                    List.map .verb newModel.verbData
                        |> Set.fromList
                        |> (Set.diff <|
                                Set.fromList model.gameSettings.verbs
                           )
            in
            case Set.isEmpty verbsLeft of
                True ->
                    let
                        ( m, _ ) =
                            Play.init newModel.gameSettings newModel.verbData
                    in
                    { newModel | page = Playing m }

                False ->
                    -- There are still verbs left to be downloaded
                    newModel



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        LoadingAvailableVerbs page ->
            viewLoadingAvailableVerbs page

        PickingSettings ->
            PickingSettings.view model.pickingSettingsModel
                |> Html.map PickingSettingsMsg

        PreparingGame ->
            viewPreparingGame model

        Playing m ->
            Play.view m
                |> Html.map PlayMsg


viewPreparingGame : Model -> Html Msg
viewPreparingGame model =
    h1 [] [ text "Preparing game..." ]


viewLoadingAvailableVerbs : LoadingAvailableVerbs -> Html Msg
viewLoadingAvailableVerbs msg =
    case msg of
        Just errorMsg ->
            div [] [ text "Failed to load verbs :(" ]

        Nothing ->
            Components.Loading.viewLoading



-- Helpers


fetchErrorToString : Http.Error -> String
fetchErrorToString error =
    "Error loading JSON"



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Playing playModel ->
            Sub.map PlayMsg (Play.subscriptions playModel)

        _ ->
            Sub.none
