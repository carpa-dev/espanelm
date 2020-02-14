module Translate.Paragraph exposing (..)

import String
import Translate.Article exposing (Articles)


type alias Paragraph =
    String


type alias Phrase =
    String


type alias ArticleBody =
    List Paragraph


toLines : Paragraph -> List Phrase
toLines s =
    -- remove last '. '
    s |> sanitize |> String.split ". " |> List.filter (\a -> String.length a > 1)


sanitize : String -> String
sanitize s =
    String.replace "..." "…" s


areParagraphsEquivalent : Paragraph -> Paragraph -> Bool
areParagraphsEquivalent p1 p2 =
    List.length (toLines p1) == List.length (toLines p2)


areArticleBodyEquivalent : ArticleBody -> ArticleBody -> Bool
areArticleBodyEquivalent a1 a2 =
    let
        p1 =
            breakArticleBody a1

        p2 =
            breakArticleBody a2
    in
    List.length p1 == List.length p2


breakArticleBody : ArticleBody -> List Phrase
breakArticleBody a1 =
    List.map toLines a1 |> List.foldr (++) []
