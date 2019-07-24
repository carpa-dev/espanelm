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
import Url exposing (Url)



---- MODEL ----


type Page
    = Home
    | Game Game.Model
    | NotFound


type alias Model =
    { page : Page
    , key : Nav.Key
    , isMenuOpen : Bool
    }


initialModel : Nav.Key -> Model
initialModel navigationKey =
    { page = Home
    , key = navigationKey
    , isMenuOpen = False
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    setNewPage (Routes.match url) (initialModel key)



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged (Maybe Routes.Route)
    | GameMsg Game.Msg
    | ToggleMenu


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

        ( ToggleMenu, _ ) ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

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
        [ div[ class (pageToClass model.page)] [ viewNavbar model
        , node "main" [] [ viewContent model.page ]]
        
        
        ]
    }


viewNavbar : Model -> Html Msg
viewNavbar model =
    nav [ class ("navbar card " ++ navbarClass model.page) ]
        [ div [ class "container" ]
            [ div [ class "navbar-brand" ]
                [ div [ class "navbar-item" ] [ text "espanelm" ]
                , viewMenuButton model
                ]
            , div [ class (withMenuClass "navbar-menu" model) ]
                [ div [ class "navbar-end" ]
                    [ a [ class "navbar-item", href <| Routes.toUrl Routes.Home, onClick ToggleMenu ] [ text "Home" ]
                    , a [ class "navbar-item", href <| Routes.toUrl Routes.Play, onClick ToggleMenu ] [ text "Play" ]
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



pageToClass: Page -> String
pageToClass page =
        case page of
                Home ->
                        "home-page"

                Game _ ->
                         "game-page"

                NotFound ->
                        "not-found-page"

navbarClass : Page -> String
navbarClass page = 
        case page of
                Home ->
                        "is-transparent"
                _ ->
                        "is-black"

