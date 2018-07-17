module SectionEditor exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Debug exposing (crash, log)
import DragAndDropEvents exposing (onDragStart, onDragOver, onDragEnd, onDrop)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    { section : Section
    , movingPost : Maybe Post
    , draggedOverPost : Maybe Post
    , droppedOnPost : Maybe Post
    , accordionState : Accordion.State
    }


posts : List Post
posts =
    [ { approved = True
      , id = 1
      , links = []
      , pubDate = ""
      , submitter = 1
      , url = "http://site.tld"
      , title = "Post One"
      , position = 1
      , blurb = "1 comes first"
      }
    , { approved = True
      , id = 2
      , links = []
      , pubDate = ""
      , submitter = 2
      , url = "http://site.tld"
      , title = "Post Two"
      , position = 2
      , blurb = "2 is goofy"
      }
    , { approved = True
      , id = 3
      , links = []
      , pubDate = ""
      , submitter = 3
      , url = "http://site.tld"
      , title = "Post Three"
      , position = 3
      , blurb = "3 is pretty good"
      }
    , { approved = True
      , id = 4
      , links = []
      , pubDate = ""
      , submitter = 4
      , url = "http://site.tld"
      , title = "Post Four"
      , position = 4
      , blurb = "Four is the best"
      }
    ]


init : ( Model, Cmd Msg )
init =
    ( { section =
            { id = 1
            , name = "First Section"
            , posts = posts
            , position = 1
            }
      , movingPost = Nothing
      , draggedOverPost = Nothing
      , droppedOnPost = Nothing
      , accordionState = Accordion.initialState
      }
    , Cmd.none
    )



-- Update


type Msg
    = DragStart Post
    | DragEnd
    | DropOn Post
    | DragOver Post
    | AccordionMsg Accordion.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart post ->
            ( { model
                | movingPost = Just post
              }
            , Cmd.none
            )

        DragEnd ->
            ( { model
                | movingPost = Nothing
                , draggedOverPost = Nothing
              }
            , Cmd.none
            )

        DropOn post ->
            let
                droppedOnModel =
                    moveMovingPost model post
            in
                ( { droppedOnModel
                    | droppedOnPost = Just post
                  }
                , Cmd.none
                )

        DragOver post ->
            ( { model
                | draggedOverPost = Just post
              }
            , Cmd.none
            )

        AccordionMsg state ->
            ( { model | accordionState = state }
            , Cmd.none
            )


moveMovingPost : Model -> Post -> Model
moveMovingPost model overThis =
    -- why doesn't this piping shit work?
    --
    -- removeMovingPost model
    --     |> insertMovingPost overThis
    --     |> renumberPosts
    let
        x =
            removeMovingPost model

        y =
            insertMovingPost x overThis

        z =
            renumberPosts y
    in
        z


renumberPosts : Model -> Model
renumberPosts model =
    let
        renumber idx post =
            { post | position = idx + 1 }

        section =
            model.section

        newSection =
            { section
                | posts = List.indexedMap renumber section.posts
            }
    in
        { model
            | section = newSection
        }


removeMovingPost : Model -> Model
removeMovingPost model =
    let
        removeThis =
            case model.movingPost of
                Nothing ->
                    Debug.crash ("stupid")

                Just movingPost ->
                    movingPost

        section =
            model.section

        newSection =
            { section
                | posts = List.filter (\p -> p /= removeThis) model.section.posts
            }
    in
        { model
            | section = newSection
        }


insertMovingPost : Model -> Post -> Model
insertMovingPost model overThis =
    let
        movingPost =
            model.movingPost

        actualMovingPost =
            case movingPost of
                Nothing ->
                    Debug.crash "what a joke"

                Just movingPost ->
                    movingPost

        head =
            List.take (overThis.position - 1) model.section.posts

        tail =
            List.drop (overThis.position - 1) model.section.posts

        isNotActualMovingPost post =
            post /= actualMovingPost

        headless =
            List.filter isNotActualMovingPost head

        tailless =
            List.filter isNotActualMovingPost tail

        section =
            model.section

        newSection =
            { section
                | posts = headless ++ [ actualMovingPost ] ++ tailless
            }
    in
        { model
            | section = newSection
        }



-- View


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ Accordion.config AccordionMsg
                    |> Accordion.withAnimation
                    |> Accordion.cards [ sectionHeader model ]
                    |> Accordion.view model.accordionState
                ]
            ]
        ]


sectionHeader : Model -> Accordion.Card Msg
sectionHeader model =
    Accordion.card
        { id = toString model.section.id
        , options = []
        , header =
            Accordion.header [] <|
                Accordion.toggle []
                    [ text
                        (model.section.name
                            ++ "("
                            ++ toString (List.length model.section.posts)
                            ++ ")"
                        )
                    ]
        , blocks = [ Accordion.block [] (postsView model) ]
        }


postsView : Model -> List (Block.Item Msg)
postsView model =
    let
        sortedPosts =
            List.sortBy .position model.section.posts
    in
        List.map (\p -> postView p) sortedPosts


postView : Post -> Block.Item Msg
postView post =
    Block.custom
        (Card.config
            [ Card.attrs
                [ attribute "draggable" "true"
                , onDragOver <| DragOver post
                , onDragStart <| DragStart post
                , onDragEnd <| DragEnd
                , onDrop <| DropOn post
                ]
            ]
            |> Card.block []
                [ Block.text [] [ text post.title ] ]
            |> Card.view
        )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Accordion.subscriptions model.accordionState AccordionMsg
