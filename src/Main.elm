module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Issue exposing (Issue, decodeIssue, emptyIssue)
import Post exposing (Post)
import Section exposing (Section)


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
    { issue : Issue
    , status : String
    , accordionState : Accordion.State
    }


init : ( Model, Cmd Msg )
init =
    ( { issue = emptyIssue -- TODO - smells
      , status = ""
      , accordionState = Accordion.initialState
      }
    , (getIssue 515)
    )



-- Update


type Msg
    = LoadIssue (Result Http.Error Issue)
    | AccordionMsg Accordion.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadIssue (Ok issue) ->
            ( { model | issue = issue }, Cmd.none )

        LoadIssue (Err msg) ->
            ( { model | status = toString msg }, Cmd.none )

        AccordionMsg state ->
            ( { model | accordionState = state }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ text model.status
                , renderIssue model.issue model.accordionState
                ]
            ]
        ]


renderIssue : Issue -> Accordion.State -> Html Msg
renderIssue issue accordionState =
    Accordion.config AccordionMsg
        |> Accordion.withAnimation
        |> Accordion.cards (List.map renderSection issue.sections)
        |> Accordion.view accordionState


renderSection : Section -> Accordion.Card Msg
renderSection section =
    Accordion.card
        { id = section.name
        , options = []
        , header =
            Accordion.header [] <|
                Accordion.toggle []
                    [ text
                        (section.name
                            ++ "("
                            ++ toString (List.length section.posts)
                            ++ ")"
                        )
                    ]
        , blocks =
            [ Accordion.block [] (List.map renderPost section.posts) ]
        }


renderPost :
    Post
    -> Block.Item msg -- TODO what is `msg`?
renderPost post =
    Block.custom
        (Card.config [ Card.attrs [ attribute "draggable" "true" ] ]
            |> Card.block []
                [ Block.text [] [ text post.title ] ]
            |> Card.view
        )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Accordion.subscriptions model.accordionState AccordionMsg



-- HTTP


getIssue : Int -> Cmd Msg
getIssue id =
    let
        url =
            "http://bulletin.aashe.org/api/issue/" ++ toString id ++ "/"
    in
        Http.send LoadIssue (Http.get url decodeIssue)
