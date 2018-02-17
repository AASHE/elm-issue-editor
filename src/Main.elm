import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http exposing (..)
import Utils

import Issue exposing (Issue, decodeIssue)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Model


type alias Model =
    { issues : List Issue
    , status : String
    }


init : ( Model, Cmd Msg )
init =
    (Model [] "Initial", (getIssue 515))



-- Update


type Msg
    = NewIssue (Result Http.Error Issue)
    | InitIssue



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewIssue (Ok issue) ->
            ({ model | issues = List.append model.issues [issue] }, Cmd.none)
        NewIssue (Err msg) ->
            ({ model | status = toString msg }, Cmd.none)
        InitIssue ->
            (model, getIssue 515)


-- View


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick InitIssue ] [ text "Load Data" ]
        , text model.status
        , div [] ( List.map renderIssue model.issues )
        ]

renderIssue issue =
    ul [] [ li [] [ text issue.pubDate ]
          , div [] ( List.map renderSection issue.sections )
          ]

renderSection section =
    ul [] [ li [] [ text section.name ] ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- HTTP

getIssue : Int -> Cmd Msg
getIssue id =
    let
        url = "http://bulletin.aashe.org/api/issue/" ++ toString id ++ "/"
    in
        Http.send NewIssue (Http.get url decodeIssue)
