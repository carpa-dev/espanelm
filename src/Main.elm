module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Game.Game as Game
import Home
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)
import NotFound
import Routes
import Url exposing (Url)



---- MODEL ----


type Page
    = Home
    | Game Game.Model
    | NotFound


type alias Model =
    { page : Page
    , key : Nav.Key
    }


initialModel : Nav.Key -> Model
initialModel navigationKey =
    { page = Home
    , key = navigationKey
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    setNewPage (Routes.match url) (initialModel key)



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged (Maybe Routes.Route)
    | GameMsg Game.Msg


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            ( { model | page = Home }, Cmd.none )

        Just Routes.Play ->
            let
                ( m, cmd ) =
                    Game.init
            in
            ( { model | page = Game m }, Cmd.map GameMsg cmd )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            setNewPage url model

        ( GameMsg playMsg, Game playModel ) ->
            let
                ( pageModel, pageCmd ) =
                    Game.update playMsg playModel
            in
            ( { model | page = Game pageModel }, Cmd.map GameMsg pageCmd )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


viewContent : Page -> Html Msg
viewContent page =
    case page of
        Home ->
            Home.view

        Game gameModel ->
            Game.view gameModel |> Html.map GameMsg

        NotFound ->
            NotFound.view


view : Model -> Browser.Document Msg
view model =
    { title = "Espanelm"
    , body =
        [ div []
            [ h1 [] [ text "espanelm" ]
            , a [ href <| Routes.toUrl Routes.Home ] [ text "link to home | " ]
            , a [ href <| Routes.toUrl Routes.Play ] [ text "i am the play page" ]
            , div [] [ viewContent model.page ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = Routes.match >> UrlChanged
        , onUrlRequest = LinkClicked
        }
