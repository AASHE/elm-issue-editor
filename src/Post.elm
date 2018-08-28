module Post exposing (..)

-- import Json.Encode

import Json.Decode as JD
import Json.Decode.Pipeline exposing (decode, required)


-- @TODO - decode dates into Date.Date's, not strings
-- @TODO - encode dates Date.Date's into strings in same format API gives them

type Post
    = Event Int String String Bool String Int Int String String String String String String
    | Job Int String String Bool String Int Int String String
    | NewResource Int String String Bool String Int Int String String
    | Opportunity Int String String Bool String Int Int String String
    | Story Int String String Bool String Int Int String String String


postDecoder : JD.Decoder Post
postDecoder =
    JD.field "post_type" JD.string
        |> JD.andThen postFromPostType


postFromPostType : String -> JD.Decoder Post
postFromPostType postType =
    case postType of
        "EVENT" ->
            eventDecoder

        "JOB" ->
            jobDecoder

        "NEWRESOURCE" ->
            newResourceDecoder

        "OPPORTUNITY" ->
            opportunityDecoder

        "STORY" ->
            storyDecoder

        _ ->
            JD.fail ("Invalid type of post: " ++ postType)


eventDecoder : JD.Decoder Post
eventDecoder =
    decode Event
        |> required "id" JD.int
        |> required "title" JD.string
        |> required "url" JD.string
        |> required "approved" JD.bool
        |> required "pub_date" JD.string
        |> required "submitter" JD.int
        |> required "position" JD.int
        |> required "post_type" JD.string
        |> required "startDate" JD.string
        |> required "endDate" JD.string
        |> required "time" JD.string
        |> required "organization" JD.string
        |> required "location" JD.string


jobDecoder : JD.Decoder Post
jobDecoder =
    decode Job
        |> required "id" JD.int
        |> required "title" JD.string
        |> required "url" JD.string
        |> required "approved" JD.bool
        |> required "pub_date" JD.string
        |> required "submitter" JD.int
        |> required "position" JD.int
        |> required "post_type" JD.string
        |> required "organization" JD.string


newResourceDecoder : JD.Decoder Post
newResourceDecoder =
    decode NewResource
        |> required "id" JD.int
        |> required "title" JD.string
        |> required "url" JD.string
        |> required "approved" JD.bool
        |> required "pub_date" JD.string
        |> required "submitter" JD.int
        |> required "position" JD.int
        |> required "post_type" JD.string
        |> required "blurb" JD.string


opportunityDecoder : JD.Decoder Post
opportunityDecoder =
    decode Opportunity
        |> required "id" JD.int
        |> required "title" JD.string
        |> required "url" JD.string
        |> required "approved" JD.bool
        |> required "pub_date" JD.string
        |> required "submitter" JD.int
        |> required "position" JD.int
        |> required "post_type" JD.string
        |> required "blurb" JD.string


storyDecoder : JD.Decoder Post
storyDecoder =
    decode Story
        |> required "id" JD.int
        |> required "title" JD.string
        |> required "url" JD.string
        |> required "approved" JD.bool
        |> required "pub_date" JD.string
        |> required "submitter" JD.int
        |> required "position" JD.int
        |> required "post_type" JD.string
        |> required "blurb" JD.string
        |> required "date" JD.string



-- type alias Event =
--     { id : Int
--     , title : String
--     , url : String
--     , approved : Bool
--     , pubDate : String
--     , submitter : Int
--     , position : Int
--     , links : List Link
--     , startDate : String
--     , endDate : String
--     , time : String
--     , location : String
--     , organization : String
--     }
-- type alias Job =
--     { id : Int
--     , title : String
--     , url : String
--     , approved : Bool
--     , pubDate : String
--     , submitter : Int
--     , position : Int
--     , links : List Link
--     , organization : String
--     }
-- type alias Opportunity =
--     { id : Int
--     , title : String
--     , url : String
--     , approved : Bool
--     , pubDate : String
--     , submitter : Int
--     , position : Int
--     , links : List Link
--     , blurb : String
--     , date : String
--     }
-- type alias NewResource =
--     { id : Int
--     , title : String
--     , url : String
--     , approved : Bool
--     , pubDate : String
--     , submitter : Int
--     , position : Int
--     , links : List Link
--     , postType : String
--     , blurb : String
--     }
-- type alias Story =
--     { id : Int
--     , title : String
--     , url : String
--     , approved : Bool
--     , pubDate : String
--     , submitter : Int
--     , position : Int
--     , links : List Link
--     , postType : String
--     , blurb : String
--     }
-- decodePost : JD.Decoder Post
-- decodePost =
--     decode Post
--         |> required "id" JD.int
--         |> required "title" JD.string
--         |> required "url" JD.string
--         |> required "approved" JD.bool
--         |> required "pub_date" JD.string
--         |> required "submitter" JD.int
--         |> required "position" JD.int
--         |> required "links" JD.list decodeLink
--         |> required "post_type" JD.string
--         |> JD.andThen postFromPostType
-- encodePost : Model -> Json.Encode.Value
-- encodePost record =
--     Json.Encode.object
--         [ ( "id", Json.Encode.int <| record.id )
--         , ( "title", Json.Encode.string <| record.title )
--         , ( "url", Json.Encode.string <| record.url )
--         , ( "approved", Json.Encode.bool <| record.approved )
--         , ( "pubDate", Json.Encode.string <| record.pubDate )
--         , ( "submitter", Json.Encode.int <| record.submitter )
--         , ( "position", Json.Encode.int <| record.position )
--         , ( "links", Json.Encode.list <| List.map encodeLink <| record.links )
--         ]
