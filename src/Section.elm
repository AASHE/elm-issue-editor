module Section exposing (..)

import Json.Encode
import Json.Decode exposing (field)
import Post exposing (..)


type alias Model =
    { id : Int
    , name : String
    , posts : List Post.Model
    , position : Int
    }


decodeSection : Json.Decode.Decoder Model
decodeSection =
    Json.Decode.map4 Model
        (field "id" Json.Decode.int)
        (field "name" Json.Decode.string)
        (field "posts" (Json.Decode.list decodePost))
        (field "position" Json.Decode.int)


encodeSection : Model -> Json.Encode.Value
encodeSection record =
    Json.Encode.object
        [ ( "id", Json.Encode.int <| record.id )
        , ( "name", Json.Encode.string <| record.name )
        , ( "posts", Json.Encode.list <| List.map encodePost <| record.posts )
        , ( "position", Json.Encode.int <| record.position )
        ]
