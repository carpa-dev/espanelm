module Game.PickingSettings exposing (Model, Msg(..), OutMsg(..), combinedDecoder, conjugationDecoder, decodeVerbs, init, inputDecoder, update, validateVerb, view)

import Bool.Extra as BoolExtra
import Form.Decoder as Decoder exposing (Decoder, Validator, custom)
import Game.GameCommon as GameCommon exposing (Conjugation, GameSettings, Person, Verb)
import Html exposing (Html, a, button, div, footer, form, h1, h3, h4, header, img, input, label, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onInput, onSubmit)
import List.Extra exposing (foldl1)
import Maybe.Extra as MaybeExtra exposing (unwrap, values)
import Result.Extra


type alias Model =
    { form : GameSettingsForm
    , availableVerbs : List Verb
    , availablePersons : List Person
    , availableConjugations : List Conjugation
    , showModal : Bool
    , modal : List Verb
    }


type alias GameSettingsForm =
    { verbs : VerbInput
    , conjugations : ConjugationsInput
    }



-- Pristine = user hasn't touched it yet
-- Dirty = user has touched but we haven't validated yet
-- Valid/Invalid = user touched, and we have validated


type VerbInput
    = VerbPristine String
    | VerbDirty String
    | VerbValid String
    | VerbInvalid String (List VerbsError)


type ConjugationsInput
    = ConjugationPristine (List Conjugation)
    | ConjugationDirty (List Conjugation)
    | ConjugationValid (List Conjugation)
    | ConjugationInvalid (List Conjugation) (List ConjugationsError)


type GameSettingsFormErrors
    = ConjugationsError ConjugationsError
    | VerbsError VerbsError


type ConjugationsError
    = ConjugationRequired


type VerbsError
    = VerbNotAvailable String
    | VerbRequired



-- Msg


type Msg
    = OnInputVerbs String
    | OnBlurVerbs
    | ToggleConjugation Conjugation Bool
    | OnSubmit
    | ToggleModal
    | ModalSelectVerb Verb
    | ModalConfirm


type OutMsg
    = Play GameSettings
    | NoOp


init : List String -> ( Model, Cmd Msg )
init loadedVerbs =
    ( { availableConjugations = GameCommon.allConjugations
      , availablePersons = GameCommon.allPersons
      , availableVerbs = loadedVerbs
      , form = initForm
      , showModal = False
      , modal = []
      }
    , Cmd.none
    )


initForm : GameSettingsForm
initForm =
    -- when initializing there's only two possible values
    -- either a field is Pristine (initial setup)
    -- or Valid (came from a previous run)
    { --verbs = initVerb gameSettings.verbs
      verbs = VerbPristine ""

    --, conjugations = ConjugationPristine gameSettings.conjugations
    , conjugations = ConjugationPristine []
    }


initVerb : List Verb -> VerbInput
initVerb verbs =
    if List.length verbs > 0 then
        VerbDirty (String.join ", " verbs)

    else
        VerbPristine ""



-- update


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        OnInputVerbs verbs ->
            ( updateFormVerbDirty model verbs, Cmd.none, NoOp )

        OnBlurVerbs ->
            -- When user blurs the input
            -- We want to validate it
            ( updateFormVerbOnBlurVerbs model, Cmd.none, NoOp )

        ToggleConjugation conjugation checked ->
            ( { model | form = updateConjugation model conjugation }, Cmd.none, NoOp )

        OnSubmit ->
            updateOnSubmit model

        ToggleModal ->
            ( { model | showModal = not model.showModal }, Cmd.none, NoOp )

        ModalConfirm ->
            let
                oldForm =
                    model.form

                newForm =
                    { oldForm | verbs = VerbValid <| verbsToString model.modal }
            in
            ( { model | showModal = not model.showModal, form = newForm, modal = [] }, Cmd.none, NoOp )

        ModalSelectVerb verb ->
            if List.member verb model.modal then
                ( { model | modal = List.filter (\v -> verb == v) model.modal }, Cmd.none, NoOp )

            else
                ( { model | modal = verb :: model.modal }, Cmd.none, NoOp )



-- Updates model when user is typing (Dirty)


setVerb : GameSettingsForm -> VerbInput -> GameSettingsForm
setVerb form verbs =
    { form | verbs = verbs }


setForm : Model -> GameSettingsForm -> Model
setForm model form =
    { model | form = form }


updateFormVerbDirty : Model -> String -> Model
updateFormVerbDirty model verb =
    setVerb model.form (VerbDirty verb)
        |> setForm model


updateFormVerbOnBlurVerbs : Model -> Model
updateFormVerbOnBlurVerbs model =
    setVerb model.form (validateVerb model.availableVerbs model.form.verbs)
        |> setForm model


updateConjugation : Model -> Conjugation -> GameSettingsForm
updateConjugation model conjugation =
    let
        form =
            model.form

        list =
            conjugationToList form.conjugations
    in
    case List.member conjugation list of
        True ->
            let
                newConj =
                    List.filter (\c -> c /= conjugation) list
            in
            { form | conjugations = validateConjugation newConj }

        False ->
            { form | conjugations = validateConjugation (conjugationToList (addConjugationToList conjugation form.conjugations)) }


addConjugationToList : Conjugation -> ConjugationsInput -> ConjugationsInput
addConjugationToList conjugation allConjugations =
    ConjugationDirty
        (conjugation
            :: conjugationToList allConjugations
        )


conjugationToList : ConjugationsInput -> List Conjugation
conjugationToList c_ =
    case c_ of
        ConjugationPristine c ->
            c

        ConjugationDirty c ->
            c

        ConjugationInvalid c _ ->
            c

        ConjugationValid c ->
            c


updateOnSubmit : Model -> ( Model, Cmd Msg, OutMsg )
updateOnSubmit model =
    let
        m =
            updateFormVerbOnBlurVerbs model

        form =
            m.form

        newForm =
            { form | conjugations = validateConjugation (conjugationToList form.conjugations) }

        newModel =
            { m | form = newForm }
    in
    case decodeEntireForm model of
        Err _ ->
            ( newModel, Cmd.none, NoOp )

        Ok gameForm ->
            ( newModel, Cmd.none, Play gameForm )


inputToString : VerbInput -> String
inputToString i_ =
    case i_ of
        VerbDirty i ->
            i

        VerbPristine i ->
            i

        VerbInvalid i _ ->
            i

        VerbValid i ->
            i



-- view


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ form [ class "panel", onSubmit OnSubmit ]
                [ div [ class "panel-heading" ] [ text "New game" ]
                , div [ class "panel-block" ]
                    [ div [ class "control" ]
                        [ label [ class "label" ] [ div [] [ text "Verbs" ], a [ onClick ToggleModal, href "" ] [ text "dont know where to start?" ] ]
                        , div [ class "field has-addons" ] [ div [ class "control" ] [ input [ class "input", placeholder "comer, dormir, regresar...", value (inputToString model.form.verbs), onBlur OnBlurVerbs, onInput OnInputVerbs ] [] ], div [ class "control" ] [ button [ class "button is-primary" ] [ text "play" ] ] ]
                        , viewFormVerbErrors model
                        ]
                    ]
                , div [ class "panel-block" ] [ viewConjugationList model ]
                , div [ class "panel-block" ] [ viewSubmitButton model ]
                ]
            ]
        , div [ class ("modal" ++ modalClass model) ] [ div [ class "modal-background", onClick ToggleModal ] [], div [ class "modal-card" ] [ header [ class "modal-card-head" ] [ p [ class "modal-card-title" ] [ text "verbs" ], button [ class "delete", onClick ToggleModal ] [] ], section [ class "modal-card-body" ] [ viewModalPanel model ], footer [ class "modal-card-foot" ] [ button [ class "button is-primary", onClick ModalConfirm ] [ text "Pick these verbs" ] ] ] ]
        ]


modalClass : Model -> String
modalClass model =
    if model.showModal then
        " is-active"

    else
        ""


viewModalPanel : Model -> Html Msg
viewModalPanel model =
    nav [ class "panel" ] [ div [ class "panel-tabs" ] [ a [ href "", class "is-active" ] [ text "all" ] ], viewModalVerbs model ]


viewModalVerbs : Model -> Html Msg
viewModalVerbs model =
    div []
        (List.map
            (\v -> label [ class "panel-block" ] [ input [ type_ "checkbox", class "panel-block", onClick (ModalSelectVerb v), checked <| isModalVerbChecked model v ] [], text v ])
            model.availableVerbs
        )


isModalVerbChecked : Model -> Verb -> Bool
isModalVerbChecked model verb =
    List.member verb model.modal


verbInputStateClass : Model -> String
verbInputStateClass model =
    case model.form.verbs of
        VerbInvalid _ _ ->
            " is-danger"

        VerbValid _ ->
            " is-success"

        _ ->
            ""


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    button [ class "button is-primary is-fullwidth" ] [ text "Play" ]


areConjugationsValid : List Conjugation -> Bool
areConjugationsValid conjugations =
    not <| List.isEmpty conjugations


isVerbEmpty : String -> Bool
isVerbEmpty verb =
    verb |> String.trim |> String.isEmpty


getNormalVerbErrors : List VerbsError -> String
getNormalVerbErrors errors =
    let
        hasNormalError =
            List.any
                (\v ->
                    case v of
                        VerbRequired ->
                            True

                        _ ->
                            False
                )
                errors
    in
    if hasNormalError then
        "This field is required (verbs)"

    else
        ""


viewFormVerbErrors : Model -> Html Msg
viewFormVerbErrors model =
    case model.form.verbs of
        VerbInvalid s errors ->
            let
                formattedErrors =
                    [ notAvailableVerbErrMsg errors ] ++ [ getNormalVerbErrors errors ]
            in
            div [ style "color" "red" ]
                (List.map
                    (\e ->
                        p [ class "help is-danger" ] [ text e ]
                    )
                    formattedErrors
                )

        _ ->
            p [ class "help" ] [ text "Type a list of verbs separated by commas" ]


notAvailableVerbErrMsg : List VerbsError -> String
notAvailableVerbErrMsg errors =
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



-- Conjugation List


viewConjugationList : Model -> Html Msg
viewConjugationList model =
    div [ class "field" ]
        [ div [ class "conjugation-title" ]
            [ span [ class "label" ] [ text "Conjugations" ]
            , viewConjugationValidation model
            ]
        , ul [ class "list" ] <|
            List.map
                (viewConjugationCheckbox model)
                model.availableConjugations
        ]


viewConjugationCheckbox : Model -> Conjugation -> Html Msg
viewConjugationCheckbox model conjugation =
    let
        text_ =
            GameCommon.conjugationToString conjugation ++ " (" ++ GameCommon.conjugationExample conjugation ++ ")"

        id_ =
            conjugationToID conjugation

        checked_ =
            isChecked model conjugation
    in
    li [ class "list-item" ]
        [ label [ class "checkbox", for (conjugationToID conjugation) ] [ input [ type_ "checkbox", id id_, checked checked_, onCheck (ToggleConjugation conjugation) ] [], text text_ ]
        ]


isChecked : Model -> Conjugation -> Bool
isChecked m c =
    List.member c (conjugationToList m.form.conjugations)


conjugationToID : Conjugation -> String
conjugationToID c =
    c
        |> GameCommon.conjugationToString
        |> String.replace " " "-"
        |> String.toLower


viewConjugationValidation : Model -> Html Msg
viewConjugationValidation model =
    case model.form.conjugations of
        ConjugationInvalid errors _ ->
            p [ class "help is-danger" ] [ text "Please pick at least one conjugation" ]

        _ ->
            p [ class "help is-danger is-invisible" ] [ text "Please pick at least one conjugation" ]



-- helpers


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


inputToListVerb : VerbInput -> List Verb
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



-- Form


inputDecoder : Decoder (List String) VerbsError (List String)
inputDecoder =
    Decoder.custom
        (\s ->
            let
                joined =
                    s |> String.join ""
            in
            if String.length joined <= 0 then
                Err [ VerbRequired ]

            else
                Ok s
        )


decodeSingleVerbAvailability : List String -> Decoder String VerbsError String
decodeSingleVerbAvailability availableVerbs =
    Decoder.custom
        (validateVerbAvailability availableVerbs)


decodeVerbs : List String -> Decoder (List String) VerbsError (List String)
decodeVerbs availableVerbs =
    Decoder.list (decodeSingleVerbAvailability availableVerbs)


combinedDecoder : List String -> Decoder (List String) VerbsError (List String)
combinedDecoder availableVerbs =
    inputDecoder
        |> Decoder.andThen (\_ -> decodeVerbs availableVerbs)



-- form helpers


validateVerbAvailability :
    List String
    -> String
    -> Result (List VerbsError) String
validateVerbAvailability availableVerbs v =
    if List.member v availableVerbs then
        Ok v

    else
        Err [ VerbNotAvailable v ]


filterFilledVerb : Verb -> List String
filterFilledVerb s =
    s |> String.split "," |> List.map String.trim |> List.filter (not << String.isEmpty)



-- Public api


conjugationDecoder : Decoder (List Conjugation) ConjugationsError ()
conjugationDecoder =
    Decoder.custom
        (\c ->
            if List.isEmpty c then
                Err [ ConjugationRequired ]

            else
                Ok ()
        )



-- Given a list of available verbs, and a VerbInput
-- Validates and return the new input


validateVerb : List String -> VerbInput -> VerbInput
validateVerb availableVerbs input_ =
    let
        input =
            inputToString input_

        decoded =
            decodeFormVerb availableVerbs input_
    in
    case decoded of
        Err err ->
            VerbInvalid input err

        Ok _ ->
            VerbValid input


validateConjugation : List Conjugation -> ConjugationsInput
validateConjugation c_ =
    let
        decoded =
            decodeConjugation c_
    in
    case decoded of
        Ok c ->
            ConjugationValid c_

        Err err ->
            ConjugationInvalid c_ err


decodeFormVerb : List Verb -> VerbInput -> Result (List VerbsError) (List String)
decodeFormVerb availableVerbs input =
    Decoder.run
        (combinedDecoder availableVerbs)
        (filterFilledVerb (String.toLower <| inputToString input))


decodeConjugation : List Conjugation -> Result (List ConjugationsError) ConjugationsInput
decodeConjugation conjugations =
    if List.isEmpty conjugations then
        Err [ ConjugationRequired ]

    else
        Ok (ConjugationValid conjugations)


decodeEntireForm : Model -> Result (List GameSettingsFormErrors) GameSettings
decodeEntireForm model =
    let
        brandNewForm =
            { verbs = [], conjugations = [] }
    in
    Ok brandNewForm |> decodeConjugationForForm model |> decodeVerbForForm model


decodeVerbForForm : Model -> Result (List GameSettingsFormErrors) GameSettings -> Result (List GameSettingsFormErrors) GameSettings
decodeVerbForForm model form =
    let
        verbs =
            decodeFormVerb model.availableVerbs model.form.verbs
    in
    case verbs of
        Err e ->
            Err (applyWithError (\e_ -> e_ ++ wrapVerbError e) form)

        Ok v ->
            Ok (\f -> { f | verbs = v }) |> Result.Extra.andMap form


applyWithError : (List a -> List a) -> Result (List a) b -> List a
applyWithError wrapFn result =
    case result of
        Err err ->
            wrapFn err

        Ok _ ->
            wrapFn []


decodeConjugationForForm : Model -> Result (List GameSettingsFormErrors) GameSettings -> Result (List GameSettingsFormErrors) GameSettings
decodeConjugationForForm model form =
    let
        conjugation =
            decodeConjugation (conjugationToList model.form.conjugations)
    in
    case conjugation of
        Err e ->
            Err (applyWithError (\e_ -> e_ ++ wrapConjugationError e) form)

        Ok c ->
            Ok (\f -> { f | conjugations = conjugationToList c }) |> Result.Extra.andMap form


wrap : (a -> b) -> a -> b
wrap f a =
    f a


wrapList : List a -> (a -> b) -> List b
wrapList a f =
    List.map (wrap f) a


wrapVerbError : List VerbsError -> List GameSettingsFormErrors
wrapVerbError err =
    wrapList err VerbsError


wrapConjugationError : List ConjugationsError -> List GameSettingsFormErrors
wrapConjugationError err =
    wrapList err ConjugationsError
