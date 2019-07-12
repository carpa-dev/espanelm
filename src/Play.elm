module Play exposing (Model, Msg, init, update, view)

import Html exposing (Html, a, button, div, h1, img, input, li, text, ul)
import Html.Attributes exposing (disabled, href, placeholder, src, style, value)
import Html.Events exposing (onBlur, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, map2, string)



-- Model


type alias Verb =
    String


type VerbOptions
    = Failure
    | Loading
    | Success (List Verb)


type alias Model =
    { verbs : String
    , options : VerbOptions
    , unavailable : List String
    , failureCause : String
    }


initialModel : Model
initialModel =
    { verbs = ""
    , options = Loading
    , unavailable = []
    , failureCause = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Http.get { url = "/verbs.json", expect = Http.expectJson GotVerbOptions verbOptionsDecoder } )


type Msg
    = Change String
    | Blur
    | GotVerbOptions (Result Http.Error (List Verb))



-- update


getUnavailableVerbs : Model -> List String
getUnavailableVerbs model =
    model.verbs |> String.split "," |> List.filter (\verb -> not (isVerbAvailable model.options verb))


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

                Err r ->
                    ( { model
                        | options = Failure
                        , failureCause =
                            fetchErrorToString r
                      }
                    , Cmd.none
                    )


viewVerbsList : Model -> Html Msg
viewVerbsList model =
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
    if hasEmptyVerb model then
        div [ style "color" "red" ] [ text "There are empty verbs on your list" ]

    else
        text ""


viewUnavailableVerb : Model -> Html Msg
viewUnavailableVerb model =
    if not (List.isEmpty model.unavailable) then
        div [ style "color" "red" ] [ text ("The following verbs are not available: " ++ String.join ", " model.unavailable) ]

    else
        text ""


viewOptionItem : Verb -> Html Msg
viewOptionItem verb =
    li [] [ text verb ]


hasEmptyVerb : Model -> Bool
hasEmptyVerb model =
    model.verbs
        |> String.split ","
        |> List.any isVerbEmpty


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


getInputBorderStyle : Model -> String
getInputBorderStyle model =
    if String.isEmpty model.verbs || isValid model then
        "1px solid grey"

    else
        "1px solid red"


verbOptionsDecoder : Decoder (List Verb)
verbOptionsDecoder =
    list string


fetchErrorToString : Http.Error -> String
fetchErrorToString error =
    case error of
        Http.BadBody err ->
            err

        _ ->
            "wrong"


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Choose your verbs" ]
        , input [ placeholder "Verbs", value model.verbs, onBlur Blur, onInput Change, style "border" (getInputBorderStyle model) ] []
        , button [ disabled (not (isValid model)) ] [ text "Submit" ]
        , viewUnavailableVerb model
        , viewVerbsList model
        ]
