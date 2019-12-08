module Translate exposing (Model, Msg, init, initCmd, update, view)

import Html exposing (Html, a, button, div, figure, footer, h1, h4, header, img, input, li, p, section, span, text, textarea, ul)
import Html.Attributes exposing (class, disabled, href, placeholder, src, value)
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
    { publishedAt : Int
    , image : String
    , ptbr : Article
    , es : Article
    }


type alias ArticlesList =
    List Articles


type TranslationType
    = EsToPtbr
    | PtBrToEs


type alias Translation =
    { originalArticle : Article
    , translatedArticle : Article
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
    | ClickedStartTranslation Articles TranslationType
    | TypedTranslation String
    | ReviewAnswer Translation
    | RedoAnswer Translation
    | Continue Translation
    | ClickedBackToList


initCmd : Cmd Msg
initCmd =
    load GotNews


initTranslation : Articles -> TranslationType -> Translation
initTranslation articles translationType =
    -- original -> translated are flipped here
    { originalArticle = getOriginalArticle translationType articles
    , translatedArticle = getTargetArticle translationType articles

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

        ClickedStartTranslation article translationType ->
            ( { model | translation = Started (initTranslation article translationType) }, Cmd.none )

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

        RedoAnswer t ->
            -- rollback last answer
            let
                newTranslation =
                    Started { t | answers = removeLast t.answers, answer = "" }
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
    List.length t.originalArticle.body <= t.currentIndex


removeLast : List string -> List string
removeLast t =
    let
        reversed =
            List.reverse t
    in
    case reversed of
        [] ->
            []

        h :: tail ->
            List.reverse tail



-- view


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container full-height" ]
            [ case model.data of
                LoadError ->
                    viewLoadError model

                Loaded a ->
                    viewMain model a

                NotLoaded ->
                    div [] []
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
            valueAtOrDefault "Nothing else left" t.currentIndex t.originalArticle.body

        totalParagraph =
            List.length t.originalArticle.body |> String.fromInt

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
            valueAtOrDefault "Nothing else left" translation.currentIndex translation.translatedArticle.body


getActionButton : PlayingStatus -> Translation -> Html Msg
getActionButton playingStatus translation =
    case playingStatus of
        PSPlaying ->
            div [ class "field is-grouped" ]
                [ div [ class "control" ] [ button [ disabled True, class "button", onClick <| RedoAnswer translation ] [ text "Try again" ] ]
                , div [ class "control is-fullwidth is-expanded" ]
                    [ button [ class "button is-primary is-fullwidth", onClick <| ReviewAnswer translation ] [ text "Compare Answer" ]
                    ]
                ]

        PSReviewing ->
            div [ class "field is-grouped" ]
                [ div [ class "control" ] [ button [ class "button", onClick <| RedoAnswer translation ] [ text "Try again" ] ]
                , div [ class "control is-fullwidth is-expanded" ]
                    [ button [ class "button is-primary is-fullwidth", onClick <| Continue translation ] [ text "Next paragraph" ]
                    ]
                ]


viewTranslationGame : PlayingStatus -> Translation -> Html Msg
viewTranslationGame playingStatus translation =
    let
        reference =
            getReference playingStatus translation

        actionButton =
            getActionButton playingStatus translation
    in
    div [ class "full-height translations-wrapper" ]
        [ h1 [ class "title" ] [ text translation.originalArticle.title ]
        , viewCurrentTranslation translation
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
                        (List.map (\t -> p [] [ text t ]) finishedTranslation.translatedArticle.body)
                    ]
                ]
            ]
        , button [ class "button is-primary", onClick ClickedBackToList ] [ text "Back to articles list" ]
        ]


viewArticlesList : ArticlesList -> Html Msg
viewArticlesList articlesList =
    div [ class "article-list" ]
        [ h4 [ class "title" ] [ text "Brazilian Portuguese to Spanish" ]
        , ul [ class "list is-hoverable" ]
            [ div [ class "columns is-multiline" ]
                (List.map
                    (viewArticleInList PtBrToEs)
                    articlesList
                )
            ]
        ]


viewArticleInList : TranslationType -> Articles -> Html Msg
viewArticleInList translationType articles =
    div [ class "column is-one-third" ]
        [ div [ class "card article-card" ]
            [ div [ class "card-image" ]
                [ figure [ class "image is-4by3" ]
                    [ img [ src articles.image ] []
                    ]
                ]
            , div [ class "card-content" ]
                [ p [ class "title is-6" ] [ text (getOriginalArticle translationType articles).title ]
                , viewHowManyParagraphs (getOriginalArticle translationType articles)
                ]
            , footer [ class "card-footer" ]
                [ button [ class "button is-primary is-fullwidth is-medium", onClick (ClickedStartTranslation articles translationType) ] [ text "START" ]
                ]
            ]
        ]


viewHowManyParagraphs : Article -> Html Msg
viewHowManyParagraphs article =
    p [ class "subtitle is-6" ] [ text ((String.fromInt <| List.length article.body) ++ " paragraph(s)") ]


viewLoadError : Model -> Html Msg
viewLoadError model =
    h1 [] [ text "Failed to load data" ]


getOriginalArticle : TranslationType -> Articles -> Article
getOriginalArticle translationType articles =
    case translationType of
        PtBrToEs ->
            articles.ptbr

        EsToPtbr ->
            articles.es


getTargetArticle : TranslationType -> Articles -> Article
getTargetArticle translationType articles =
    case translationType of
        PtBrToEs ->
            articles.es

        EsToPtbr ->
            articles.ptbr



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
        |> required "publishedAt" int
        |> required "image" string
        |> required "ptbr"
            individualArticleDec
        |> required "es"
            individualArticleDec


listArticlesDecoder : Decoder (List Articles)
listArticlesDecoder =
    Decode.list articlesDecoder
