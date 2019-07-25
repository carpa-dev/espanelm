module Home exposing (view)

import Html exposing (Html, a, div, h1, h2, section, span, text)
import Html.Attributes exposing (class, href, target)
import Routes


view : Html msg
view =
    section [ class "home-hero" ]
        [ div [ class "hero-body" ] [ div [ class "container" ] [ div [ class "column" ] [ h1 [ class "title" ] [ text "Espanelm" ], h2 [ class "subtitle" ] [ a [ class "subtitle", href "https://expresionesyrefranes.com/2007/05/21/pedirle-peras-al-olmo", target "_blank" ] [ text "don't ask the elm for pears." ], span [] [ text " practice your spanish here." ] ], div [] [ a [ class "button is-primary is-medium is-fullwidth", href <| Routes.toUrl Routes.Play ] [ text "Play" ] ] ] ] ]
        ]
