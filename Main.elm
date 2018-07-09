module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Debug exposing (crash, log)
import DragAndDropEvents exposing (onDragStart, onDragOver, onDragEnd, onDrop)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
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
    , section : Maybe Section
    , accordionState : Accordion.State
    , movingPost : Maybe Post
    , draggedOverPost : Maybe Post
    , droppedOnPost : Maybe Post
    , sectionsToSave : List Section
    , showSaveButton : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { issue = emptyIssue -- <-- smells to me
      , section = Nothing
      , status = ""
      , accordionState = Accordion.initialState
      , movingPost = Nothing
      , draggedOverPost = Nothing
      , droppedOnPost = Nothing
      , sectionsToSave = []
      , showSaveButton = False
      }
    , (getIssue 515)
    )



-- Update


type Msg
    = LoadIssue (Result Http.Error Issue)
    | AccordionMsg Accordion.State
    | DragStart Post
    | DragEnd
    | DropOn Post
    | DragOver Post
    | SaveSectionOrder Section


getSectionForPost : Model -> Post -> Section
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


removePostFromSection : Section -> Post -> Section
removePostFromSection section post =
    let
        remainingPosts =
            List.filter (\p -> p /= post) section.posts

        postsSlidUp =
            slidePostsUp remainingPosts post

        _ =
            Debug.log "remainingPosts" remainingPosts
    in
        { section | posts = postsSlidUp }


slidePostsUp : List Post -> Post -> List Post
slidePostsUp posts post =
    List.map
        (\p ->
            if p.position >= post.position then
                { p | position = p.position - 1 }
            else
                p
        )
        posts


removePostFromIssue : Model -> Issue -> Post -> Issue
removePostFromIssue model issue post =
    let
        sections =
            List.map (\s -> removePostFromSection s post) issue.sections
    in
        { issue | sections = sections }


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

        DragStart post ->
            let
                issue =
                    removePostFromIssue model model.issue post

                section =
                    getSectionForPost model post
            in
                ( { model
                    | movingPost = Just post
                    , issue = issue
                    , section = Just section
                  }
                , Cmd.none
                )

        DragEnd ->
            ( { model
                | movingPost = Nothing
                , section = Nothing
              }
            , Cmd.none
            )

        DropOn post ->
            let
                movingPost =
                    case model.movingPost of
                        Nothing ->
                            Debug.crash "No model.movingPost"

                        Just movingPost ->
                            movingPost

                _ = Debug.log "section.posts before"
                    (List.map (\p -> p.title ++ ": " ++
                               toString p.position) section.posts)

                section =
                    case model.section of
                        Nothing ->
                            Debug.crash "No sectino to drop post on"

                        Just section ->
                            insertPostInSection section movingPost post

                sectionsToSave =
                    if List.member section model.sectionsToSave then
                        model.sectionsToSave
                    else
                        List.append model.sectionsToSave [ section ]
            in
                ( { model
                    | droppedOnPost = Just post
                    , sectionsToSave = sectionsToSave
                  }
                , Cmd.none
                )

        DragOver post ->
            ( { model | draggedOverPost = Just post }
            , Cmd.none
            )

        SaveSectionOrder section ->
            -- Eventually make call to server to save order.
            -- For now, just remove section from model.sectionsToSave.
            ( { model
                | sectionsToSave =
                    List.filter
                        (\s -> s /= section)
                        model.sectionsToSave
              }
            , Cmd.none
            )


slidePostsDown : List Post -> List Post
slidePostsDown posts =
    List.map (\p -> { p | position = p.position + 1 }) posts


dummyPost : Post
dummyPost =
    { id = -1
    , title = "dummy post"
    , url = ""
    , approved = False
    , pubDate = ""
    , submitter = -1
    , position = -1
    , links = []
    }


getPostForMaybePost : Maybe Post -> Post
getPostForMaybePost maybe =
    case maybe of
        Nothing ->
            -- this can't be correct
            dummyPost

        Just maybe ->
            maybe


insertPostInSection : Section -> Post -> Post -> Section
insertPostInSection section insertThis beforeThis =
    let
        head =
            List.take (beforeThis.position - 1) section.posts

        tail =
            List.drop beforeThis.position section.posts

        downslidTail =
            slidePostsDown tail

        insertThisPositionedPost =
            { insertThis | position = beforeThis.position }

        posts =
            head ++ [ insertThisPositionedPost ] ++ downslidTail

        _ =
            List.map
                (\p ->
                    Debug.log "post"
                        (p.title ++ ": " ++ toString p.position)
                )
                posts
    in
        { section
            | posts = posts
        }



-- View


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ text model.status
                , viewIssue model
                ]
            ]
        ]


viewIssue : Model -> Html Msg
viewIssue model =
    Accordion.config AccordionMsg
        |> Accordion.withAnimation
        |> Accordion.cards (viewSections model)
        |> Accordion.view model.accordionState


viewSections : Model -> List (Accordion.Card Msg)
viewSections model =
    let
        viewThisModelsSection =
            viewSection model
    in
        List.map viewThisModelsSection model.issue.sections


viewSection : Model -> Section -> Accordion.Card Msg
viewSection model section =
    let
        showSaveButton =
            List.member section model.sectionsToSave
    in
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
                        , if showSaveButton then
                            button
                                [ onClick (SaveSectionOrder section) ]
                                [ text "Save" ]
                          else
                            text ""
                        ]
            , blocks =
                [ Accordion.block [] (viewPosts model section) ]
            }


viewPosts : Model -> Section -> List (Block.Item Msg)
viewPosts model section =
    let
        viewThisSectionsPosts =
            viewPost model

        sortedPosts =
            List.sortBy .position section.posts
    in
        List.map viewThisSectionsPosts sortedPosts


viewPost :
    Model
    -> Post
    -> Block.Item Msg
viewPost model post =
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



-- HTTP


getIssue : Int -> Cmd Msg
getIssue id =
    let
        url =
            "http://bulletin.aashe.org/api/issue/" ++ toString id ++ "/"
    in
        Http.send LoadIssue (Http.get url decodeIssue)
