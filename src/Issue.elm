module Issue exposing(..)

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline

import Section exposing (Section, decodeSection, encodeSection)

type alias Issue =
    { id : Int
    , pubDate : String
    , sections : List Section
    , name : String
    , subject : String
    , fromName : String
    , fromEmail : String
    , replyToEmail : String
    , organizationName : String
    , addressLine1 : String
    , addressLine2 : String
    , addressLine3 : String
    , city : String
    , state : String
    , internationalState : String
    , postalCode : String
    , country : String
    , htmlTemplateName : String
    , textTemplateName : String
    }


emptyIssue =
    { id = 0
    , pubDate = ""
    , sections = []
    , name = ""
    , subject = ""
    , fromName = ""
    , fromEmail = ""
    , replyToEmail = ""
    , organizationName = ""
    , addressLine1 = ""
    , addressLine2 = ""
    , addressLine3 = ""
    , city = ""
    , state = ""
    , internationalState = ""
    , postalCode = ""
    , country = ""
    , htmlTemplateName = ""
    , textTemplateName = ""
    }

decodeIssue : Json.Decode.Decoder Issue
decodeIssue =
    Json.Decode.Pipeline.decode Issue
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "pub_date" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "sections" (Json.Decode.list decodeSection)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "from_name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "from_email" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "reply_to_email" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "organization_name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "address_line_2" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "address_line_2" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "address_line_3" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "city" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "state" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "international_state" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "postal_code" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "country" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "html_template_name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "text_template_name" (Json.Decode.string)

encodeIssue : Issue -> Json.Encode.Value
encodeIssue record =
    Json.Encode.object
        [ ("id",  Json.Encode.int <| record.id)
        , ("pubDate",  Json.Encode.string <| record.pubDate)
        , ("sections",  Json.Encode.list <| List.map encodeSection <| record.sections)
        , ("name",  Json.Encode.string <| record.name)
        , ("subject",  Json.Encode.string <| record.subject)
        , ("fromName",  Json.Encode.string <| record.fromName)
        , ("fromEmail",  Json.Encode.string <| record.fromEmail)
        , ("replyToEmail",  Json.Encode.string <| record.replyToEmail)
        , ("organizationName",  Json.Encode.string <| record.organizationName)
        , ("addressLine1",  Json.Encode.string <| record.addressLine1)
        , ("addressLine2",  Json.Encode.string <| record.addressLine2)
        , ("addressLine3",  Json.Encode.string <| record.addressLine3)
        , ("city",  Json.Encode.string <| record.city)
        , ("state",  Json.Encode.string <| record.state)
        , ("internationalState",  Json.Encode.string <| record.internationalState)
        , ("postalCode",  Json.Encode.string <| record.postalCode)
        , ("country",  Json.Encode.string <| record.country)
        , ("htmlTemplateName",  Json.Encode.string <| record.htmlTemplateName)
        , ("textTemplateName",  Json.Encode.string <| record.textTemplateName)
        ]
