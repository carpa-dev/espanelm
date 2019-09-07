module Translate exposing (Model, Msg, init, initCmd, update, view)

import Html exposing (Html, div, h1, text)
import Http
import Json.Decode as Decode exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias Article =
    { lang : String
    , title : String
    , body : List String
    }


type alias Articles =
    { ptbr : Article
    , es : Article
    }


type alias LatestNews =
    List Articles


type alias Model =
    { status : Status
    }


type Status
    = NotLoaded
    | LoadError
    | Loaded LatestNews


init =
    { status = NotLoaded
    }


type Msg
    = GotNews (Result Http.Error LatestNews)


initCmd : Cmd Msg
initCmd =
    load GotNews


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNews r ->
            case r of
                Ok a ->
                    ( { model | status = Loaded a }, Cmd.none )

                Err a ->
                    ( { model | status = LoadError }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [] []


viewLoadError : Model -> Html Msg
viewLoadError model =
    h1 [] [ text "Failed to load data" ]



-- Http loading


load : (Result Http.Error (List Articles) -> msg) -> Cmd msg
load a =
    Http.get
        { url = "/news.json"
        , expect = Http.expectJson a listArticlesDecoder
        }


decoder : Decoder (List String)
decoder =
    list string


individualArticleDec : Decoder Article
individualArticleDec =
    Decode.succeed Article
        |> required "lang" string
        |> required "title" string
        |> required "body" (list string)


articlesDecoder : Decoder Articles
articlesDecoder =
    Decode.succeed Articles
        |> required "ptbr"
            individualArticleDec
        |> required "es"
            individualArticleDec


listArticlesDecoder : Decoder (List Articles)
listArticlesDecoder =
    Decode.list articlesDecoder
