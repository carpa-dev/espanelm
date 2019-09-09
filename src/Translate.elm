module Translate exposing (Model, Msg, init, initCmd, update, view)

import Html exposing (Html, a, button, div, h1, input, li, p, section, span, text, textarea, ul)
import Html.Attributes exposing (class, disabled, href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra


type alias Article =
    { lang : String
    , title : String
    , body : List String
    }


type alias Articles =
    { ptbr : Article
    , es : Article
    }


type alias ArticlesList =
    List Articles


type alias Translation =
    { spanishArticle : Article
    , portugueseArticle : Article
    , currentIndex : Int
    , answer : String
    , answers : List String
    }


type TranslationStatus
    = NotStarted
    | Started Translation
    | Reviewing Translation
    | Finished Translation



-- narrow down types above


type PlayingStatus
    = PSPlaying
    | PSReviewing


type alias Model =
    { data : DataStatus
    , translation : TranslationStatus
    }


type DataStatus
    = NotLoaded
    | LoadError
    | Loaded ArticlesList


init : Model
init =
    { data = NotLoaded
    , translation = NotStarted
    }


type Msg
    = GotNews (Result Http.Error ArticlesList)
    | ClickedTitle Articles
    | TypedTranslation String
    | ReviewAnswer Translation
    | Continue Translation
    | ClickedBackToList


initCmd : Cmd Msg
initCmd =
    load GotNews


initTranslation : Articles -> Translation
initTranslation articles =
    { spanishArticle = articles.es
    , portugueseArticle = articles.ptbr

    -- TODO:
    -- Technically the array will never be empty
    , currentIndex = 0
    , answer = ""
    , answers = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNews r ->
            case r of
                Ok a ->
                    ( { model | data = Loaded a }, Cmd.none )

                Err a ->
                    ( { model | data = LoadError }, Cmd.none )

        ClickedTitle article ->
            ( { model | translation = Started (initTranslation article) }, Cmd.none )

        TypedTranslation typed ->
            case model.translation of
                Started t ->
                    let
                        newTranslation =
                            Started { t | answer = typed }
                    in
                    ( { model | translation = newTranslation }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReviewAnswer t ->
            let
                newTranslation =
                    Reviewing { t | answers = t.answers ++ [ t.answer ] }
            in
            ( { model | translation = newTranslation }, Cmd.none )

        Continue t ->
            let
                newTranslation =
                    { t | currentIndex = t.currentIndex + 1, answers = t.answers ++ [ t.answer ], answer = "" }
            in
            if hasFinished newTranslation then
                ( { model | translation = Finished newTranslation }, Cmd.none )

            else
                ( { model | translation = Started newTranslation }, Cmd.none )

        ClickedBackToList ->
            ( { model | translation = NotStarted }, Cmd.none )


hasFinished : Translation -> Bool
hasFinished t =
    List.length t.spanishArticle.body <= t.currentIndex



-- view


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ case model.data of
                LoadError ->
                    viewLoadError model

                Loaded a ->
                    viewMain model a

                NotLoaded ->
                    div [] [ text "Loading..." ]
            ]
        ]


viewMain : Model -> ArticlesList -> Html Msg
viewMain model articlesList =
    case model.translation of
        NotStarted ->
            viewArticlesList articlesList

        Started translation ->
            viewTranslationGame PSPlaying translation

        Reviewing translation ->
            viewTranslationGame PSReviewing translation

        Finished finishedTranslation ->
            viewFinishedTranslation finishedTranslation


valueAtOrDefault : a -> Int -> List a -> a
valueAtOrDefault default index list =
    case List.Extra.getAt index list of
        Nothing ->
            default

        Just b ->
            b


viewUserTranslation : Translation -> Html Msg
viewUserTranslation t =
    p [] [ text (valueAtOrDefault "Nothing else left" t.currentIndex t.answers) ]


viewReferenceTranslation : Translation -> Html Msg
viewReferenceTranslation t =
    div [ class "column" ]
        [ div [ class "field" ]
            [ div [ class "control" ]
                [ textarea [ placeholder "write the translation", onInput TypedTranslation, value t.answer, class "textarea" ] []
                ]
            ]
        ]


viewCurrentTranslation : Translation -> Html Msg
viewCurrentTranslation t =
    let
        paragraph =
            valueAtOrDefault "Nothing else left" t.currentIndex t.spanishArticle.body

        totalParagraph =
            List.length t.spanishArticle.body |> String.fromInt

        currentParagraph =
            String.fromInt <| t.currentIndex + 1
    in
    div [ class "box" ]
        [ p [] [ text <| paragraph ++ " (" ++ currentParagraph ++ "/" ++ totalParagraph ++ ")" ]
        ]


getReference : PlayingStatus -> Translation -> String
getReference playingStatus translation =
    case playingStatus of
        PSPlaying ->
            ""

        PSReviewing ->
            valueAtOrDefault "Nothing else left" translation.currentIndex translation.portugueseArticle.body


getActionButton : PlayingStatus -> Translation -> Html Msg
getActionButton playingStatus translation =
    case playingStatus of
        PSPlaying ->
            button [ class "button is-primary", onClick <| ReviewAnswer translation ] [ text "Compare answer" ]

        PSReviewing ->
            button [ class "button is-primary", onClick <| Continue translation ] [ text "Next paragraph" ]


viewTranslationGame : PlayingStatus -> Translation -> Html Msg
viewTranslationGame playingStatus translation =
    let
        reference =
            getReference playingStatus translation

        actionButton =
            getActionButton playingStatus translation
    in
    div []
        [ viewCurrentTranslation translation
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "field" ]
                    [ div [ class "control" ]
                        [ textarea [ placeholder "write the translation", onInput TypedTranslation, value translation.answer, class "textarea" ] []
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "field" ]
                    [ div [ class "control" ]
                        [ textarea
                            [ disabled True
                            , value reference
                            , class "textarea"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , actionButton
        ]


viewFinishedTranslation : Translation -> Html Msg
viewFinishedTranslation finishedTranslation =
    div [ class "finished-translation" ]
        [ h1 [ class "title is-3" ] [ text "Compare your full translation with the refference one" ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "box" ]
                    [ span [ class "tag is-info" ] [ text "Yours" ]
                    , div []
                        (List.map (\t -> p [] [ text t ]) finishedTranslation.answers)
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "box" ]
                    [ span [ class "tag is-primary" ] [ text "Reference" ]
                    , div []
                        (List.map (\t -> p [] [ text t ]) finishedTranslation.portugueseArticle.body)
                    ]
                ]
            ]
        , button [ class "button is-primary", onClick ClickedBackToList ] [ text "Back to articles list" ]
        ]


viewArticlesList : ArticlesList -> Html Msg
viewArticlesList articlesList =
    ul [ class "list is-hoverable" ]
        (List.map
            (\articles ->
                a [ class "list-item", href "#", onClick (ClickedTitle articles) ] [ text (articles.es.title ++ " (" ++ (String.fromInt <| List.length articles.es.body) ++ " paragraphs)") ]
            )
            articlesList
        )


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
