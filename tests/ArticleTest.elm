module ArticleTest exposing (suite)

import Expect
import Json.Decode as Decode
import Test exposing (..)
import Translate.Article as Article


suite : Test
suite =
    describe "Decoder"
        [ test "decode individual article" <|
            \() ->
                let
                    input =
                        """
                        {
                          "lang": "pt-br",
                          "title": "my-title",
                          "body": ["bla"]
                        }
                        """

                    decodedOutput =
                        Decode.decodeString Article.individualArticleDec input
                in
                Expect.equal decodedOutput
                    (Ok
                        { lang = "pt-br"
                        , title = "my-title"
                        , body = [ "bla" ]
                        }
                    )
        ]
