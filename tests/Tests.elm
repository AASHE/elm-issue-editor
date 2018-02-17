import Http
import Test exposing (..)
import Expect

import Post


all : Test
all =
    describe "Elm Issue Editor Test Suite"
        [ test "Decodes a Post" <|
            \() ->
                Expect.equal getPost 1799
        , test "Encodes a Post" <|
            \() ->
                Expect.equal True False
        ]

postId = 1799

getPost =
    let
        request = Http.get "http://bulletin.aashe.org/api/post/1799/" Post.decodePost
    in
        Http.send request
