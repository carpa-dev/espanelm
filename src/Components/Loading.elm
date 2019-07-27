module Components.Loading exposing (viewLoading)

import Html exposing (Html, div, progress, text)
import Html.Attributes exposing (class)


viewLoading : Html msg
viewLoading =
    div [ class "loading-wrapper" ] [ progress [ class "progress is-primary is-small loading" ] [ text "" ] ]
