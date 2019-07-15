module Game.AvailableVerbs exposing (load)

import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, list, string)


load : (Result Http.Error (List String) -> msg) -> Cmd msg
load a =
    Http.get { url = "/verbs/list.json", expect = Http.expectJson a decoder }


decoder : Decoder (List String)
decoder =
    list string
