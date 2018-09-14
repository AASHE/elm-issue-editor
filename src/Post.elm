module Post exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode
import Maybe.Extra


type alias Model =
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


decodePost : JD.Decoder Model
decodePost =
    decode Model
        |> required "id" JD.int
        |> required "title" JD.string
        |> required "url" JD.string
        |> required "approved" JD.bool
        |> required "pub_date" JD.string
        |> required "submitter" JD.int
        |> required "position" JD.int
        |> required "post_type" JD.string
        |> optional "start_date" (JD.map Just JD.string) Nothing
        |> optional "end_date" (JD.map Just JD.string) Nothing
        |> optional "time" (JD.map Just JD.string) Nothing
        |> optional "organization" (JD.map Just JD.string) Nothing
        |> optional "location" (JD.map Just JD.string) Nothing
        |> optional "organization" (JD.map Just JD.string) Nothing
        |> optional "blurb" (JD.map Just JD.string) Nothing
        |> optional "date" (JD.map Just JD.string) Nothing


encodePost : Model -> Json.Encode.Value
encodePost model =
    [ Just ( "id", Json.Encode.int model.id )
    , Just ( "title", Json.Encode.string model.title )
    , Just ( "url", Json.Encode.string model.url )
    , Just ( "approved", Json.Encode.bool model.approved )
    , Just ( "pubDate", Json.Encode.string model.pubDate )
    , Just ( "submitter", Json.Encode.int model.submitter )
    , Just ( "position", Json.Encode.int model.position )
    , Just ( "postType", Json.Encode.string model.postType )
    , Maybe.map ((,) "startDate" << Json.Encode.string) model.startDate
    , Maybe.map ((,) "endDate" << Json.Encode.string) model.endDate
    , Maybe.map ((,) "time" << Json.Encode.string) model.time
    , Maybe.map ((,) "organization" << Json.Encode.string) model.organization
    , Maybe.map ((,) "location" << Json.Encode.string) model.location
    , Maybe.map ((,) "organization" << Json.Encode.string) model.organization
    , Maybe.map ((,) "blurb" << Json.Encode.string) model.blurb
    , Maybe.map ((,) "date" << Json.Encode.string) model.date
    ]
        |> Maybe.Extra.values
        |> Json.Encode.object
