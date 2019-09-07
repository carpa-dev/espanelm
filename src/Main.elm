module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Game.Game as Game
import Home
import Html exposing (Html, a, button, div, h1, nav, node, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import NotFound
import Routes
import Translate
import Url exposing (Url)



---- MODEL ----


type Page
    = Home
    | Game
    | NotFound
    | Translate


type alias Model =
    { page : Page
    , key : Nav.Key
    , isMenuOpen : Bool
    , gameModel : Game.Model
    , translateModel : Translate.Model
    }


initialModel : Nav.Key -> Model
initialModel navigationKey =
    { page = Home
    , key = navigationKey
    , isMenuOpen = False
    , gameModel = Game.init
    , translateModel = Translate.init
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    setNewPage (Routes.match url) (initialModel key)



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged (Maybe Routes.Route)
    | GameMsg Game.Msg
    | TranslateMsg Translate.Msg
    | ToggleMenu


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            ( { model | page = Home }, Cmd.none )

        Just Routes.Play ->
            ( { model | page = Game }, Cmd.map GameMsg (Game.initCmd model.gameModel) )

        Just Routes.Translate ->
            ( { model | page = Translate }, Cmd.map TranslateMsg Translate.initCmd )

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

        ( GameMsg playMsg, _ ) ->
            let
                ( pageModel, pageCmd ) =
                    Game.update playMsg model.gameModel
            in
            ( { model | page = Game, gameModel = pageModel }, Cmd.map GameMsg pageCmd )

        ( TranslateMsg translateMsg, _ ) ->
            let
                ( pageModel, pageCmd ) =
                    Translate.update translateMsg model.translateModel
            in
            ( { model | page = Translate, translateModel = pageModel }, Cmd.none )

        ( ToggleMenu, _ ) ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )



---- VIEW ----


viewContent : Model -> Html Msg
viewContent model =
    case model.page of
        Home ->
            Home.view

        Game ->
            Game.view model.gameModel |> Html.map GameMsg

        NotFound ->
            NotFound.view

        Translate ->
            Translate.view model.translateModel |> Html.map TranslateMsg


view : Model -> Browser.Document Msg
view model =
    { title = "Espanelm"
    , body =
        [ div [ class (pageToClass model.page) ]
            [ viewNavbar model
            , node "main" [] [ viewContent model ]
            ]
        ]
    }


viewNavbar : Model -> Html Msg
viewNavbar model =
    nav [ class ("navbar" ++ navbarClass model.page) ]
        [ div [ class "container" ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href <| Routes.toUrl Routes.Home ] [ text "espanelm" ]

                -- , viewMenuButton model
                ]
            , div [ class (withMenuClass "navbar-menu" model) ]
                [ div [ class "navbar-end" ]
                    [ a [ class (navbarItemClass model isHomePage), href <| Routes.toUrl Routes.Home ] [ text "Home" ]
                    , a [ class (navbarItemClass model isGamePage), href <| Routes.toUrl Routes.Play ] [ text "Play" ]
                    ]
                ]
            ]
        ]


viewMenuButton : Model -> Html Msg
viewMenuButton model =
    button
        [ class (withMenuClass "navbar-burger burger" model)
        , onClick ToggleMenu
        ]
        [ span [] []
        , span [] []
        , span [] []
        ]


withMenuClass : String -> Model -> String
withMenuClass baseClasses model =
    if model.isMenuOpen then
        baseClasses ++ " is-active"

    else
        baseClasses


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Game ->
            Sub.map GameMsg (Game.subscriptions model.gameModel)

        _ ->
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


pageToClass : Page -> String
pageToClass page =
    case page of
        Home ->
            "home-page"

        Game ->
            "game-page"

        NotFound ->
            "not-found-page"

        Translate ->
            "translate-page"


navbarClass : Page -> String
navbarClass page =
    case page of
        Home ->
            " is-transparent"

        _ ->
            " is-black card"


navbarItemClass : Model -> (Model -> Bool) -> String
navbarItemClass model isPageActive =
    let
        baseClass =
            "navbar-item is-tab"
    in
    if isPageActive model then
        baseClass ++ " is-active"

    else
        baseClass


isHomePage : Model -> Bool
isHomePage model =
    case model.page of
        Home ->
            True

        _ ->
            False


isGamePage : Model -> Bool
isGamePage model =
    case model.page of
        Game ->
            True

        _ ->
            False
