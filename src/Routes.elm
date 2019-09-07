module Routes exposing (Route(..), match, toUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Home
    | Play
    | Translate


match : Url -> Maybe Route
match url =
    Parser.parse routes url


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Play (Parser.s "play")
        , Parser.map Translate (Parser.s "translate")
        ]


toUrl : Route -> String
toUrl route =
    case route of
        Home ->
            "/"

        Play ->
            "/play"

        Translate ->
            "/translate"
