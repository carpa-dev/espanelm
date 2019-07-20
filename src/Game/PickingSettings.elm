module Game.PickingSettings exposing (Input(..), Model, Msg(..), combinedDecoder, decodeVerbs, init, inputDecoder, update, validateVerb, view)

import Bool.Extra as BoolExtra
import Form.Decoder as Decoder exposing (Decoder, Validator, custom)
import Game.GameCommon as GameCommon exposing (Conjugation, GameSettings, Person, Verb)
import Html exposing (Html, a, button, div, h1, h3, h4, img, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import List.Extra exposing (foldl1)
import Maybe.Extra as MaybeExtra exposing (unwrap, values)


type alias Model =
    { form : GameSettingsForm
    , unavailable : List String
    , conjugations : List Conjugation
    , available : Available
    }


type alias GameSettingsForm =
    { verbs : Input
    , conjugations : List Conjugation
    }



-- Pristine = user hasn't touched it yet
-- Dirty = user has touched but we haven't validated yet
-- Valid/Invalid = user touched, and we have validated


type Input
    = Pristine
    | Dirty String
    | Valid String
    | Invalid String (List VerbError)



-- | Valid String
-- | Invalid String


type alias Available =
    { conjugations : List Conjugation
    , persons : List Person
    , verbs : List Verb
    }


type Msg
    = Change String
    | Blur
    | SelectedAllOptions
    | SelectConjugation Conjugation
    | OnSubmit


init : List String -> GameSettings -> ( Model, Cmd Msg )
init loadedVerbs gameSettings =
    ( { unavailable = []
      , available =
            { conjugations = GameCommon.allConjugations
            , persons = GameCommon.allPersons
            , verbs = loadedVerbs
            }
      , conjugations = gameSettings.conjugations
      , form = initForm gameSettings
      }
    , Cmd.none
    )


initForm : GameSettings -> GameSettingsForm
initForm gameSettings =
    -- when initializing there's only two possible values
    -- either a field is Pristine (initial setup)
    -- or Valid (came from a previous run)
    { verbs = initVerb gameSettings.verbs
    , conjugations = gameSettings.conjugations
    }


initVerb : List Verb -> Input
initVerb verbs =
    if List.length verbs > 0 then
        Dirty (String.join ", " verbs)

    else
        Pristine



-- update
-- all verbs are available


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        Change verbs ->
            updateFormVerbDirty model verbs

        Blur ->
            updateFormVerbOnBlur model

        SelectedAllOptions ->
            -- should be handled by parent
            ( model, Cmd.none )

        SelectConjugation conjugation ->
            ( updateConjugation model conjugation, Cmd.none )

        OnSubmit ->
            updateFormVerbOnBlur model



-- Updates model when user is typing (Dirty)


updateFormVerbDirty : Model -> String -> ( Model, Cmd Msg )
updateFormVerbDirty model verb =
    let
        form =
            model.form

        newForm =
            { form | verbs = Dirty verb }
    in
    ( { model | form = newForm }, Cmd.none )



-- Update model when user has stopped typing (OnBlur)


updateFormVerbOnBlur : Model -> ( Model, Cmd Msg )
updateFormVerbOnBlur model =
    let
        form =
            model.form

        newForm =
            { form | verbs = validateVerb model.available.verbs model.form.verbs }
    in
    ( { model | form = newForm }, Cmd.none )


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
        , input [ placeholder "Verbs", value (inputToString model.form.verbs), onBlur Blur, onInput Change, style "border" (getInputBorderStyle model) ] []
        , viewSubmitButton model
        , viewFormVerbErrors model
        , viewConjugationList model
        ]


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    button [ onClick OnSubmit ] [ text "Play" ]



--Ok ok ->
--    button [ onClick SelectedAllOptions ] [ text "Play" ]
-- let
--     action =
--         BoolExtra.ifElse (SelectedAllOptions) OnSubmit (isFormValid model)
-- in
--case convertFormToGameSettings model.form of
--    Just gameSettings ->
--        button [ disabled False, onClick (SelectedAllOptions gameSettings) ] [ text "Play" ]
--   Nothing ->


inputToString : Input -> String
inputToString field =
    case field of
        Pristine ->
            ""

        Dirty s ->
            s

        Valid s ->
            s

        Invalid s _ ->
            s



--Valid a ->
--    a
--Invalid a ->
--    a


maybeVerbToString : Maybe Verb -> String
maybeVerbToString verb =
    case verb of
        Just v ->
            v

        Nothing ->
            ""



-- style helpers


getInputBorderStyle : Model -> String
getInputBorderStyle model =
    "1px solid grey"


isValid : Model -> Bool
isValid model =
    True



--    areConjugationsValid model.form.conjugations && isInputValid model.form.verbs


areConjugationsValid : List Conjugation -> Bool
areConjugationsValid conjugations =
    not <| List.isEmpty conjugations


isVerbEmpty : String -> Bool
isVerbEmpty verb =
    verb |> String.trim |> String.isEmpty


getNotAvailableVerbErrors : List VerbError -> String
getNotAvailableVerbErrors errors =
    let
        notAvailableErrors =
            List.map
                (\v ->
                    case v of
                        VerbNotAvailable err ->
                            Just err

                        _ ->
                            Nothing
                )
                errors
    in
    case MaybeExtra.values notAvailableErrors of
        [] ->
            ""

        _ ->
            "Following verbs are not available: " ++ String.join ", " (MaybeExtra.values notAvailableErrors)


getNormalVerbErrors : List VerbError -> String
getNormalVerbErrors errors =
    let
        hasNormalError =
            List.any
                (\v ->
                    case v of
                        MinimumLength ->
                            True

                        _ ->
                            False
                )
                errors
    in
    if hasNormalError then
        "This field is required"

    else
        ""


viewFormVerbErrors : Model -> Html Msg
viewFormVerbErrors model =
    case model.form.verbs of
        Invalid s errors ->
            let
                formattedErrors =
                    [ getNotAvailableVerbErrors errors ] ++ [ getNormalVerbErrors errors ]
            in
            div [ style "color" "red" ]
                (List.map
                    (\e ->
                        div [] [ text e ]
                    )
                    formattedErrors
                )

        _ ->
            text ""


validateVerb : List String -> Input -> Input
validateVerb availableVerbs input =
    let
        valid =
            Decoder.run
                (combinedDecoder availableVerbs)
                (Debug.log "breakString"
                    (breakString
                        (inputToString input)
                    )
                )
    in
    case Debug.log "valid" valid of
        Err err ->
            Invalid (inputToString input) err

        Ok _ ->
            Valid (inputToString input)


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
            model.available.conjugations
        )


viewCheckedCheckbox : Model -> Conjugation -> Bool
viewCheckedCheckbox model conjugation =
    List.member conjugation model.conjugations



-- helpers


isReadyToPlay : Model -> Bool
isReadyToPlay model =
    isValid model && (List.length model.conjugations > 0)


getUnavailableVerbs : List String -> List String -> List String
getUnavailableVerbs availableVerbs verbsToBeChecked =
    List.filter (\verb -> not (List.member verb availableVerbs)) verbsToBeChecked


isVerbAvailable : List String -> String -> Bool
isVerbAvailable availableVerbs verb =
    availableVerbs
        |> List.member (String.trim verb)


verbsToString : List Verb -> String
verbsToString verbs =
    String.join ", " verbs


inputToListVerb : Input -> List Verb
inputToListVerb verbs =
    let
        l =
            verbs |> inputToString |> String.split "," |> List.filter String.isEmpty
    in
    case l of
        [] ->
            [ "" ]

        _ ->
            l


validateConjugations : List Conjugation -> Maybe (List Conjugation)
validateConjugations conjugations =
    if List.isEmpty conjugations then
        Nothing

    else
        Just conjugations


type VerbNotAvailableError
    = String


type VerbError
    = MinimumLength
    | VerbNotAvailable String


isFormValid : Model -> Bool
isFormValid model =
    -- TODO
    case model.form.verbs of
        Valid _ ->
            True

        _ ->
            False


validateVerbAvailability :
    List String
    -> String
    -> Result (List VerbError) String
validateVerbAvailability availableVerbs v =
    if List.member (Debug.log "verb being validated" v) availableVerbs then
        Ok v

    else
        Err [ VerbNotAvailable v ]


inputDecoder : Decoder (List String) VerbError (List String)
inputDecoder =
    Decoder.custom
        (\s ->
            let
                joined =
                    s |> String.join ""
            in
            if String.length joined <= 0 then
                Err [ MinimumLength ]

            else
                Ok s
        )


decodeSingleVerb : List String -> Decoder String VerbError String
decodeSingleVerb availableVerbs =
    Decoder.custom
        (validateVerbAvailability availableVerbs)


decodeVerbs : List String -> Decoder (List String) VerbError (List String)
decodeVerbs availableVerbs =
    Decoder.list (decodeSingleVerb availableVerbs)


combinedDecoder : List String -> Decoder (List String) VerbError (List String)
combinedDecoder availableVerbs =
    inputDecoder
        |> Decoder.andThen (\_ -> decodeVerbs availableVerbs)


breakString : String -> List String
breakString s =
    s |> String.split "," |> List.map String.trim |> List.filter (not << String.isEmpty)
