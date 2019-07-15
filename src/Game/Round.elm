module Game.Round exposing (Round)

import GameCommon exposing (Conjugation(..), Person(..), Verb)


type alias Round =
    { person : Person, verb : Verb, conjugation : Conjugation, answer : String }
