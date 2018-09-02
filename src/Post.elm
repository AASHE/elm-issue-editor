module Post exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode
import Maybe.Extra


type alias Post =
    { id : Int
    , title : String
    , url : String
    , approved : Bool
    , pubDate : String
    , submitter : Int
    , position : Int
    , postType : String
    , startDate : Maybe String
    , endDate : Maybe String
    , time : Maybe String
    , organization : Maybe String
    , location : Maybe String
    , organization : Maybe String
    , blurb : Maybe String
    , date : Maybe String
    }


decodePost : JD.Decoder Post
decodePost =
    decode Post
        |> required "id" JD.int
        |> required "title" JD.string
        |> required "url" JD.string
        |> required "approved" JD.bool
        |> required "pubDate" JD.string
        |> required "submitter" JD.int
        |> required "position" JD.int
        |> required "postType" JD.string
        |> optional "startDate" (JD.map Just JD.string) Nothing
        |> optional "endDate" (JD.map Just JD.string) Nothing
        |> optional "time" (JD.map Just JD.string) Nothing
        |> optional "organization" (JD.map Just JD.string) Nothing
        |> optional "location" (JD.map Just JD.string) Nothing
        |> optional "organization" (JD.map Just JD.string) Nothing
        |> optional "blurb" (JD.map Just JD.string) Nothing
        |> optional "date" (JD.map Just JD.string) Nothing


encodePost : Post -> Json.Encode.Value
encodePost record =
    [ Just ( "id", Json.Encode.int record.id )
    , Just ( "title", Json.Encode.string record.title )
    , Just ( "url", Json.Encode.string record.url )
    , Just ( "approved", Json.Encode.bool record.approved )
    , Just ( "pubDate", Json.Encode.string record.pubDate )
    , Just ( "submitter", Json.Encode.int record.submitter )
    , Just ( "position", Json.Encode.int record.position )
    , Just ( "postType", Json.Encode.string record.postType )
    , Maybe.map ((,) "startDate" << Json.Encode.string) record.startDate
    , Maybe.map ((,) "endDate" << Json.Encode.string) record.endDate
    , Maybe.map ((,) "time" << Json.Encode.string) record.time
    , Maybe.map ((,) "organization" << Json.Encode.string) record.organization
    , Maybe.map ((,) "location" << Json.Encode.string) record.location
    , Maybe.map ((,) "organization" << Json.Encode.string) record.organization
    , Maybe.map ((,) "blurb" << Json.Encode.string) record.blurb
    , Maybe.map ((,) "date" << Json.Encode.string) record.date
    ]
        |> Maybe.Extra.values
        |> Json.Encode.object
