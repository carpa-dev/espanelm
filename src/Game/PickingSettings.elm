module Game.PickingSettings exposing (Model, Msg(..), OutMsg(..), combinedDecoder, conjugationDecoder, decodeVerbs, init, inputDecoder, update, validateVerb, view)

import Bool.Extra as BoolExtra
import Form.Decoder as Decoder exposing (Decoder, Validator, custom)
import Game.GameCommon as GameCommon exposing (Conjugation, GameSettings, Person, Verb)
import Html exposing (Html, a, button, div, form, h1, h3, h4, img, input, label, li, p, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onInput, onSubmit)
import List.Extra exposing (foldl1)
import Maybe.Extra as MaybeExtra exposing (unwrap, values)
import Result.Extra


type alias Available =
    { conjugations : List Conjugation
    , persons : List Person
    , verbs : List Verb
    }


type alias Model =
    { form : GameSettingsForm
    , available : Available
    }


type alias GameSettingsForm =
    { verbs : VerbInput
    , conjugations : ConjugationsInput
    }


type VerbInput
    = VerbPristine String -- Pristine = user hasn't touched it yet
    | VerbDirty String -- Dirty = user has touched but we haven't validated yet
    | VerbValid String -- Valid/Invalid = user touched, and we have validated
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


type Msg
    = Change String
    | Blur
    | SelectConjugation Conjugation Bool
    | OnSubmit


type OutMsg
    = Play GameSettings
    | NoOp


init : List String -> GameSettings -> ( Model, Cmd Msg )
init loadedVerbs gameSettings =
    ( { available =
            { conjugations = GameCommon.allConjugations
            , persons = GameCommon.allPersons
            , verbs = loadedVerbs
            }
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
    , conjugations = initConjugation gameSettings.conjugations
    }


initVerb : List Verb -> VerbInput
initVerb verbs =
    if List.length verbs > 0 then
        VerbDirty (String.join ", " verbs)

    else
        VerbPristine ""


initConjugation : List Conjugation -> ConjugationsInput
initConjugation conjugation =
    ConjugationPristine conjugation



-- update
-- all verbs are available


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        Change verbs ->
            ( updateFormVerbDirty model verbs, Cmd.none, NoOp )

        Blur ->
            ( updateFormVerbOnBlur model, Cmd.none, NoOp )

        SelectConjugation conjugation checked ->
            ( { model | form = updateConjugation model conjugation }, Cmd.none, NoOp )

        OnSubmit ->
            let
                m =
                    updateFormVerbOnBlur model

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


updateFormVerbOnBlur : Model -> Model
updateFormVerbOnBlur model =
    setVerb model.form (validateVerb model.available.verbs model.form.verbs)
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
    form [ onSubmit OnSubmit ]
        [ h1 [] [ text "Choose your verbs" ]
        , div [ class "field" ]
            [ input [ class ("input control" ++ verbInputStateClass model), placeholder "Verbs", value (inputToString model.form.verbs), onBlur Blur, onInput Change ] []
            , viewFormVerbErrors model
            , viewSubmitButton model
            ]
        , viewConjugationList model
        ]


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
    div [ class "control" ]
        [ button [ class "button is-primary" ] [ text "Play" ]
        , button [ class "btn btn-blue" ] [ text "play" ]
        ]



-- style helpers


getInputBorderStyle : Model -> String
getInputBorderStyle model =
    "1px solid grey"



--    areConjugationsValid model.form.conjugations && isInputValid model.form.verbs


areConjugationsValid : List Conjugation -> Bool
areConjugationsValid conjugations =
    not <| List.isEmpty conjugations


isVerbEmpty : String -> Bool
isVerbEmpty verb =
    verb |> String.trim |> String.isEmpty


getNotAvailableVerbErrors : List VerbsError -> String
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
                    [ getNotAvailableVerbErrors errors ] ++ [ getNormalVerbErrors errors ]
            in
            div [ style "color" "red" ]
                (List.map
                    (\e ->
                        p [ class "help is-danger" ] [ text e ]
                    )
                    formattedErrors
                )

        _ ->
            p [ class "help" ] [ text "  _" ]


conjugationToID : Conjugation -> String
conjugationToID conjugation =
    conjugation
        |> GameCommon.conjugationToString
        |> String.replace " " "-"
        |> String.toLower


viewConjugationCheckbox : String -> Model -> Conjugation -> Html Msg
viewConjugationCheckbox t model conjugation =
    li []
        [ label [ for (conjugationToID conjugation), class "chechbox" ]
            [ input [ type_ "checkbox", id (conjugationToID conjugation), checked (viewCheckedCheckbox model conjugation), onCheck (SelectConjugation conjugation) ] []
            , text t
            ]
        ]


viewConjugationSettings : Model -> Conjugation -> Html Msg
viewConjugationSettings model conjugation =
    viewConjugationCheckbox (GameCommon.conjugationToString conjugation) model conjugation


viewConjugationValidation : Model -> Html Msg
viewConjugationValidation model =
    case model.form.conjugations of
        ConjugationInvalid errors _ ->
            div [ style "color" "red" ] [ text "please pick a conjugation" ]

        _ ->
            div [] []


viewConjugationList : Model -> Html Msg
viewConjugationList model =
    div []
        [ viewConjugationValidation
            model
        , ul []
            (List.map
                (viewConjugationSettings
                    model
                )
                model.available.conjugations
            )
        ]


viewCheckedCheckbox : Model -> Conjugation -> Bool
viewCheckedCheckbox model conjugation =
    List.member conjugation (conjugationToList model.form.conjugations)



-- helpers


isReadyToPlay : Model -> Bool
isReadyToPlay model =
    True


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
        (filterFilledVerb (inputToString input))


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
        input =
            inputToString model.form.verbs

        verbs =
            Decoder.run
                (combinedDecoder model.available.verbs)
                (filterFilledVerb input)
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
