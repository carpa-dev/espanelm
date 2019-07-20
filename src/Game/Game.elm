module Game.Game exposing (Model, Msg, init, update, view)

import Debug
import Game.AvailableVerbs
import Game.GameCommon exposing (Conjugation, GameSettings, Person, Verb)
import Game.PickingSettings as PickingSettings exposing (Msg(..))
import Game.Play as Play exposing (Msg(..))
import Game.VerbData as VerbData exposing (VerbData)
import Html exposing (Html, a, button, div, h1, h3, h4, img, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, map2, string)



-- Model


type alias LoadingAvailableVerbs =
    Maybe String


type Page
    = LoadingAvailableVerbs LoadingAvailableVerbs
    | PickingSettings PickingSettings.Model
    | PreparingGame -- Loading verbs, generating rounds
    | Playing Play.Model -- Actually playing the game


type alias Model =
    { page : Page
    , gameSettings : GameSettings
    , verbData : List VerbData
    , availableVerbs : List String
    }


initialModel : Model
initialModel =
    { page = LoadingAvailableVerbs Nothing
    , gameSettings = { verbs = [], conjugations = [] }
    , verbData = []
    , availableVerbs = []
    }


type Msg
    = GotVerbOptions (Result Http.Error (List Verb))
    | PlayMsg Play.Msg
    | PickingSettingsMsg PickingSettings.Msg
    | GotVerbData (Result Http.Error VerbData)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Game.AvailableVerbs.load GotVerbOptions )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( PlayMsg playMsg, Playing playModel ) ->
            case playMsg of
                StopGame ->
                    let
                        ( m, cmd ) =
                            PickingSettings.init model.availableVerbs model.gameSettings
                    in
                    ( { model | page = PickingSettings m }, Cmd.map PickingSettingsMsg cmd )

                _ ->
                    let
                        ( pageModel, pageCmd ) =
                            Play.update playMsg playModel
                    in
                    ( { model | page = Playing pageModel }, Cmd.map PlayMsg pageCmd )

        ( PickingSettingsMsg psMsg, PickingSettings psModel ) ->
            case psMsg of
                --SelectedAllOptions gameSettings ->
                --    ( { model | page = PreparingGame, gameSettings = gameSettings }, VerbData.load gameSettings.verbs GotVerbData )
                _ ->
                    let
                        ( m, c ) =
                            PickingSettings.update psMsg psModel
                    in
                    ( { model | page = PickingSettings m }, Cmd.map PickingSettingsMsg c )

        ( GotVerbOptions result, _ ) ->
            case result of
                Ok availableVerbs ->
                    let
                        ( m, cmd ) =
                            PickingSettings.init availableVerbs model.gameSettings
                    in
                    ( { model | page = PickingSettings m, availableVerbs = availableVerbs }, Cmd.map PickingSettingsMsg cmd )

                Err r ->
                    ( { model
                        | page = LoadingAvailableVerbs (Just <| fetchErrorToString r)
                      }
                    , Cmd.none
                    )

        ( GotVerbData (Err _), _ ) ->
            -- TODO
            ( model, Cmd.none )

        ( GotVerbData (Ok data), _ ) ->
            let
                newModel =
                    { model | verbData = data :: model.verbData }

                toBeLoaded =
                    List.filter (\v -> data.verb /= v) model.gameSettings.verbs
            in
            case List.length toBeLoaded of
                0 ->
                    let
                        ( m, c ) =
                            Play.init newModel.gameSettings newModel.verbData
                    in
                    ( { newModel | page = Playing m }, Cmd.map PlayMsg Cmd.none )

                _ ->
                    -- still needs more verbs to be loaded
                    ( newModel, Cmd.none )

        -- We could use a (_, _)
        -- but we want the compiler to tell when new Msgs are added
        ( PlayMsg gmsg, _ ) ->
            ( model, Cmd.none )

        ( PickingSettingsMsg psMsg, _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.page of
        LoadingAvailableVerbs page ->
            viewLoadingAvailableVerbs page

        PreparingGame ->
            viewPreparingGame model

        Playing m ->
            Play.view m
                |> Html.map PlayMsg

        PickingSettings m ->
            PickingSettings.view m
                |> Html.map PickingSettingsMsg


viewPreparingGame : Model -> Html Msg
viewPreparingGame model =
    h1 [] [ text "Preparing game..." ]


viewLoadingAvailableVerbs : LoadingAvailableVerbs -> Html Msg
viewLoadingAvailableVerbs msg =
    case msg of
        Just errorMsg ->
            div [] [ text "Failed to load verbs :(" ]

        Nothing ->
            div [] [ text "Loading verbs..." ]



-- Helpers


fetchErrorToString : Http.Error -> String
fetchErrorToString error =
    "Error loading JSON"
