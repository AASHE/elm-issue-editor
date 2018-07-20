module IssueEditor exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import DragAndDropEvents exposing (onDragStart, onDragOver, onDragEnd, onDrop)
import Issue exposing (..)
import Post exposing (..)
import Section exposing (..)


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
    { issue : Issue.Model
    , currentSection : Maybe Section.Model
    , movingPost : Maybe Post.Model
    , draggedOverPost : Maybe Post.Model
    , droppedOnPost : Maybe Post.Model
    , accordionState : Accordion.State
    }


dummyPosts : List Post.Model
dummyPosts =
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


moreDummyPosts : List Post.Model
moreDummyPosts =
    [ { approved = True
      , id = 10
      , links = []
      , pubDate = ""
      , submitter = 1
      , url = "http://site.tld"
      , title = "Post OneXXX"
      , position = 1
      , blurb = "1 comes first"
      }
    , { approved = True
      , id = 20
      , links = []
      , pubDate = ""
      , submitter = 2
      , url = "http://site.tld"
      , title = "Post TwoXXX"
      , position = 2
      , blurb = "2 is goofy"
      }
    , { approved = True
      , id = 30
      , links = []
      , pubDate = ""
      , submitter = 3
      , url = "http://site.tld"
      , title = "Post ThreeXXX"
      , position = 3
      , blurb = "3 is pretty good"
      }
    , { approved = True
      , id = 40
      , links = []
      , pubDate = ""
      , submitter = 4
      , url = "http://site.tld"
      , title = "Post FourXXX"
      , position = 4
      , blurb = "Four is the best"
      }
    ]


init : ( Model, Cmd msg )
init =
    let
        dummyIssue =
            Issue.dummyIssue

        section =
            { id = 1
            , name = "First Section"
            , posts = dummyPosts
            , position = 1
            }

        secondSection =
            { id = 2
            , name = "Second Section"
            , posts = moreDummyPosts
            , position = 2
            }
    in
        ( { issue =
                { dummyIssue
                    | sections = [ section, secondSection ]
                }
          , currentSection = Just section
          , movingPost = Nothing
          , draggedOverPost = Nothing
          , droppedOnPost = Nothing
          , accordionState = Accordion.initialState
          }
        , Cmd.none
        )



-- Update


type Msg
    = DragStart Post.Model
    | DragEnd
    | DropOn Post.Model
    | DragOver Post.Model
    | AccordionMsg Accordion.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart post ->
            let
                currentSection =
                    (getSectionForPost model post)
            in
                ( { model
                    | movingPost = Just post
                    , currentSection = Just currentSection
                  }
                , Cmd.none
                )

        DragEnd ->
            ( { model
                | movingPost = Nothing
                , draggedOverPost = Nothing
                , currentSection = Nothing
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


getSectionForPost : Model -> Post.Model -> Section.Model
getSectionForPost model post =
    let
        sections =
            List.filter (\s -> List.member post s.posts)
                model.issue.sections
    in
        case List.head sections of
            Nothing ->
                Debug.crash "getSectionForPost: no section for post"

            Just section ->
                section


moveMovingPost : Model -> Post.Model -> Model
moveMovingPost model here =
    renumberPosts (insertMovingPost (removeMovingPost model) here)


removeMovingPost : Model -> Model
removeMovingPost model =
    let
        currentSection =
            case model.currentSection of
                Nothing ->
                    Debug.crash "asdfasdf"

                Just currentSection ->
                    currentSection

        thinSection =
            removeMovingPostFromSection model currentSection

        issue =
            model.issue

        newIssue =
            { issue
                | sections =
                    (List.filter (\s -> s /= currentSection) issue.sections) ++ [ thinSection ]
            }
    in
        { model
            | issue = newIssue
            , currentSection = Just thinSection
        }


removeMovingPostFromSection : Model -> Section.Model -> Section.Model
removeMovingPostFromSection model section =
    let
        movingPost =
            case model.movingPost of
                Nothing ->
                    Debug.crash "stupid"

                Just movingPost ->
                    movingPost
    in
        { section
            | posts = List.filter (\p -> p /= movingPost) section.posts
        }


insertMovingPost : Model -> Post.Model -> Model
insertMovingPost model here =
    insertMovingPostInIssue model here


insertMovingPostInIssue : Model -> Post.Model -> Model
insertMovingPostInIssue model here =
    let
        currentSection =
            case model.currentSection of
                Nothing ->
                    Debug.crash "asdf"

                Just currentSection ->
                    currentSection

        otherSections =
            List.filter (\s -> s.id /= currentSection.id) model.issue.sections

        section =
            insertMovingPostInSection model here

        issue =
            model.issue

        newIssue =
            { issue
                | sections = otherSections ++ [ section ]
            }
    in
        { model
            | issue = newIssue
            , currentSection = Just section
        }


insertMovingPostInSection : Model -> Post.Model -> Section.Model
insertMovingPostInSection model here =
    let
        currentSection =
            case model.currentSection of
                Nothing ->
                    Debug.crash "asdf"

                Just currentSection ->
                    currentSection

        head =
            List.take (here.position - 1) currentSection.posts

        tail =
            List.drop (here.position - 1) currentSection.posts

        movingPost =
            case model.movingPost of
                Nothing ->
                    Debug.crash "what a joke"

                Just movingPost ->
                    movingPost

        isNotMovingPost post =
            post /= movingPost

        headless =
            List.filter isNotMovingPost head

        tailless =
            List.filter isNotMovingPost tail

        newSection =
            { currentSection
                | posts = headless ++ [ movingPost ] ++ tailless
            }
    in
        newSection


renumberPosts : Model -> Model
renumberPosts model =
    let
        renumber idx post =
            { post | position = idx + 1 }

        currentSection =
            case model.currentSection of
                Nothing ->
                    Debug.crash "asdf"

                Just currentSection ->
                    currentSection

        newSection =
            { currentSection
                | posts = List.indexedMap renumber currentSection.posts
            }

        issue =
            model.issue

        newIssue =
            { issue
                | sections =
                    (List.filter (\s -> s.id /= currentSection.id) issue.sections) ++ [ newSection ]
            }
    in
        { model
            | currentSection = Just newSection
            , issue = newIssue
        }



-- View


view : Model -> Html Msg
view model =
    let
        sortedSections =
            List.sortBy .position model.issue.sections
    in
        Grid.container []
            [ CDN.stylesheet
            , Grid.row []
                [ Grid.col []
                    [ Accordion.config AccordionMsg
                        |> Accordion.withAnimation
                        |> Accordion.cards (List.map sectionView sortedSections)
                        |> Accordion.view model.accordionState
                    ]
                ]
            ]


sectionView : Section.Model -> Accordion.Card Msg
sectionView section =
    Accordion.card
        { id = toString section.id
        , options = []
        , header =
            Accordion.header [] <|
                Accordion.toggle []
                    [ text
                        section.name
                    ]
        , blocks = [ Accordion.block [] (postsView section.posts) ]
        }


postsView : List Post.Model -> List (Block.Item Msg)
postsView posts =
    let
        sortedPosts =
            List.sortBy .position posts
    in
        List.map (\p -> postView p) sortedPosts


postView : Post.Model -> Block.Item Msg
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
