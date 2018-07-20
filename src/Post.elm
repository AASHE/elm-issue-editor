module Post exposing (..)

import Json.Encode
import Json.Decode exposing (field)
import Json.Decode.Pipeline

import Link exposing (Link, decodeLink, encodeLink)


type alias Model =
    { id : Int
    , title : String
    , url : String
    , approved : Bool
    , pubDate : String
    , submitter : Int
    , position : Int
    , links : List Link
    }

decodePost : Json.Decode.Decoder Model
decodePost =
    Json.Decode.Pipeline.decode Model
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "title" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "approved" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "pub_date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "submitter" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "position" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "links" (Json.Decode.list decodeLink)

encodePost : Model -> Json.Encode.Value
encodePost record =
    Json.Encode.object
        [ ("id",  Json.Encode.int <| record.id)
        , ("title",  Json.Encode.string <| record.title)
        , ("url",  Json.Encode.string <| record.url)
        , ("approved",  Json.Encode.bool <| record.approved)
        , ("pubDate",  Json.Encode.string <| record.pubDate)
        , ("submitter",  Json.Encode.int <| record.submitter)
        , ("position",  Json.Encode.int <| record.position)
        , ("links",  Json.Encode.list <| List.map encodeLink <| record.links)
        ]
