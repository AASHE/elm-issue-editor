module Link exposing (..)

import Json.Encode
import Json.Decode exposing (field)


type alias Link =
    { id : Int
    , label : String
    , url : String
    , position : Int
    }


decodeLink : Json.Decode.Decoder Link
decodeLink =
    Json.Decode.map4 Link
        (field "id" Json.Decode.int)
        (field "label" Json.Decode.string)
        (field "url" Json.Decode.string)
        (field "position" Json.Decode.int)


encodeLink : Link -> Json.Encode.Value
encodeLink record =
    Json.Encode.object
        [ ("id",  Json.Encode.int <| record.id)
        , ("label",  Json.Encode.string <| record.label)
        , ("url",  Json.Encode.string <| record.url)
        , ("position",  Json.Encode.int <| record.position)
        ]
