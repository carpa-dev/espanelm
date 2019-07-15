module Play exposing (Model, Msg, init, update, view)

import Debug
import Game exposing (Msg(..))
import Game.AvailableVerbs
import Game.PickingSettings as PickingSettings exposing (Msg(..))
import GameCommon exposing (Conjugation, Person, Verb)
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
    | Playing Game.Model -- Actually playing the game


type alias Model =
    { page : Page
    }


initialModel : Model
initialModel =
    { page = LoadingAvailableVerbs Nothing
    }


type Msg
    = GotVerbOptions (Result Http.Error (List Verb))
    | GameMsg Game.Msg
    | PickingSettingsMsg PickingSettings.Msg


init : ( Model, Cmd Msg )
init =
    ( initialModel, Game.AvailableVerbs.load GotVerbOptions )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GameMsg gameMsg, Playing gameModel ) ->
            case gameMsg of
                StopGame ->
                    -- TODO
                    -- at this point settings doesn't have the state of PickingSettings page anymore
                    ( model, Cmd.none )

                _ ->
                    let
                        ( pageModel, pageCmd ) =
                            Game.update gameMsg gameModel
                    in
                    ( { model | page = Playing pageModel }, Cmd.map GameMsg pageCmd )

        ( PickingSettingsMsg psMsg, PickingSettings psModel ) ->
            case psMsg of
                Start ->
                    -- TODO: load vebs
                    ( model, Cmd.none )

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
                            PickingSettings.init availableVerbs
                    in
                    ( { model | page = PickingSettings m }, Cmd.map PickingSettingsMsg cmd )

                Err r ->
                    ( { model
                        | page = LoadingAvailableVerbs (Just <| fetchErrorToString r)
                      }
                    , Cmd.none
                    )

        -- We could use a (_, _)
        -- but we want the compiler to tell when new Msgs are added
        ( GameMsg gmsg, _ ) ->
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
            Game.view m
                |> Html.map GameMsg

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


verbOptionsDecoder : Decoder (List Verb)
verbOptionsDecoder =
    list string


fetchErrorToString : Http.Error -> String
fetchErrorToString error =
    "Error loading JSON"
