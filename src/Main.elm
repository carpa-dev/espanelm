module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, h1, img, input, li, text, ul)
import Html.Attributes exposing (disabled, placeholder, src, style, value)
import Html.Events exposing (onBlur, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, string)



---- MODEL ----


type VerbOptions
    = Failure
    | Loading
    | Success (List String)


type alias Model =
    { verbs : String
    , options : VerbOptions
    , unavailable: List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { verbs = "", options = Loading, unavailable = [] }
    , Http.get
        { url = "http://localhost:8080/conjugations.json"
        , expect = Http.expectJson GotVerbOptions verbOptionsDecoder
        }
    )


verbOptionsDecoder : Decoder (List String)
verbOptionsDecoder =
    list (field "verb" string)



---- UPDATE ----


type Msg
    = Change String
    | Blur
    | GotVerbOptions (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change verbs ->
            ( { model | verbs = verbs }, Cmd.none )

        Blur ->
            ( { model | unavailable = getUnavailableVerbs model }, Cmd.none )

        GotVerbOptions result ->
            case result of
                Ok response ->
                    ( { model | options = Success response }, Cmd.none )

                Err _ ->
                    ( { model | options = Failure }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Choose your verbs" ]
        , input [ placeholder "Verbs", value model.verbs, onBlur Blur, onInput Change, style "border" (getInputBorderStyle model) ] []
        , button [ disabled (not (isValid model)) ] [ text "Submit" ]
        -- , viewEmptyVerb model
        , viewUnavailableVerb model
        , viewOptionList model
        ]


viewOptionList : Model -> Html Msg
viewOptionList model =
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
    if hasEmptyVerb model then div [ style "color" "red" ] [ text "There are empty verbs on your list" ] else text ""


viewUnavailableVerb : Model -> Html Msg
viewUnavailableVerb model = 
    if not (List.isEmpty model.unavailable) then
        div [ style "color" "red" ] [ text ("The following verbs are not available: " ++ (String.join ", " model.unavailable)) ]
    else text ""


viewOptionItem : String -> Html Msg
viewOptionItem verb =
    li [] [ text verb ]


hasEmptyVerb : Model -> Bool
hasEmptyVerb model = 
    model.verbs |>
        String.split "," |>
        List.any isVerbEmpty


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


getUnavailableVerbs : Model -> List String
getUnavailableVerbs model = 
    model.verbs |> String.split "," |> List.filter (\verb -> not (isVerbAvailable model.options verb))


getInputBorderStyle : Model -> String
getInputBorderStyle model =
    if String.isEmpty model.verbs || isValid model then
        "1px solid grey"

    else
        "1px solid red"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
