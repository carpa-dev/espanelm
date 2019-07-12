module View exposing (notFoundView, view)

import Browser
import Html exposing (Html, h3)


view : Model -> Browser.Document Msg
view model =
    case model of
        _ ->
            { title = "Espanelm", body = notFoundView }


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]
