module Game.PickingSettings exposing (Model, Msg(..), init, update, view)

import GameCommon exposing (Conjugation, Person, Verb)
import Html exposing (Html, a, button, div, h1, h3, h4, img, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)


type alias Model =
    { verbs : String
    , availableVerbs : List String
    , unavailable : List String
    , failureCause : String
    , conjugations : List Conjugation
    , availableConjugations : List Conjugation
    , availablePersons : List Person
    }


type Msg
    = Change String
    | Blur
    | Start
    | SelectConjugation GameCommon.Conjugation


initialModel : Model
initialModel =
    { verbs = "abortar"
    , availableVerbs = []
    , unavailable = []
    , failureCause = ""
    , conjugations = []
    , availableConjugations = GameCommon.allConjugations
    , availablePersons = GameCommon.allPersons
    }


init : List String -> ( Model, Cmd Msg )
init loadedVerbs =
    ( { initialModel | availableVerbs = loadedVerbs }, Cmd.none )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        Change verbs ->
            ( { model | verbs = verbs }, Cmd.none )

        Blur ->
            ( { model | unavailable = getUnavailableVerbs model }, Cmd.none )

        Start ->
            -- should be handled by parent
            ( model, Cmd.none )

        SelectConjugation conjugation ->
            ( updateConjugation model conjugation, Cmd.none )


updateConjugation : Model -> Conjugation -> Model
updateConjugation model conjugation =
    case List.member conjugation model.conjugations of
        True ->
            let
                newConj =
                    List.filter (\c -> c /= conjugation) model.conjugations
            in
            { model | conjugations = newConj }

        False ->
            { model | conjugations = conjugation :: model.conjugations }



-- view


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Choose your verbs" ]
        , input [ placeholder "Verbs", value model.verbs, onBlur Blur, onInput Change, style "border" (getInputBorderStyle model) ] []
        , button [ disabled (not (isReadyToPlay model)), onClick Start ] [ text "Play" ]
        , viewUnavailableVerb model
        , viewConjugationList model
        ]



-- style helpers


getInputBorderStyle : Model -> String
getInputBorderStyle model =
    if String.isEmpty model.verbs || isValid model then
        "1px solid grey"

    else
        "1px solid red"


isValid : Model -> Bool
isValid model =
    (model |> getVerbList |> List.length)
        == (model.verbs |> String.split "," |> List.length)


getVerbList : Model -> List String
getVerbList model =
    model.verbs
        |> String.split ","
        |> List.filter (\verb -> not (isVerbEmpty verb) && isVerbAvailable model.availableVerbs verb)


isVerbEmpty : String -> Bool
isVerbEmpty verb =
    verb |> String.trim |> String.isEmpty


viewUnavailableVerb : Model -> Html Msg
viewUnavailableVerb model =
    if not (List.isEmpty model.unavailable) then
        div [ style "color" "red" ] [ text ("The following verbs are not available: " ++ String.join ", " model.unavailable) ]

    else
        text ""


conjugationToID : Conjugation -> String
conjugationToID conjugation =
    conjugation
        |> GameCommon.conjugationToString
        |> String.replace " " "-"
        |> String.toLower


viewConjugationCheckbox : String -> Model -> Conjugation -> Html Msg
viewConjugationCheckbox t model conjugation =
    li []
        [ input [ type_ "checkbox", id (conjugationToID conjugation), checked (viewCheckedCheckbox model conjugation) ] []
        , label [ for (conjugationToID conjugation), onClick (SelectConjugation conjugation) ] [ text t ]
        ]


viewConjugationSettings : Model -> Conjugation -> Html Msg
viewConjugationSettings model conjugation =
    viewConjugationCheckbox (GameCommon.conjugationToString conjugation) model conjugation


viewConjugationList : Model -> Html Msg
viewConjugationList model =
    ul []
        (List.map
            (viewConjugationSettings
                model
            )
            model.availableConjugations
        )


viewCheckedCheckbox : Model -> Conjugation -> Bool
viewCheckedCheckbox model conjugation =
    List.member conjugation model.conjugations



-- helpers


isReadyToPlay : Model -> Bool
isReadyToPlay model =
    isValid model && (List.length model.conjugations > 0)


getUnavailableVerbs : Model -> List String
getUnavailableVerbs model =
    model.verbs |> String.split "," |> List.filter (\verb -> not (isVerbAvailable model.availableVerbs verb))


isVerbAvailable : List String -> String -> Bool
isVerbAvailable availableVerbs verb =
    availableVerbs
        |> List.member (String.trim verb)
