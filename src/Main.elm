module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import DragAndDropEvents exposing (onDragStart, onDragOver, onDragEnd, onDrop)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    , accordionState : Accordion.State
    , movingPost : Maybe Post
    , movingPostPosition : Int
    , draggedOverPost : Maybe Post
    , droppedOnPost : Maybe Post
    , sectionsToSave : List Section
    , showSaveButton : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { issue = emptyIssue -- <-- smells to me
      , status = ""
      , accordionState = Accordion.initialState
      , movingPost = Nothing
      , movingPostPosition = -1
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
            ( { model
                | movingPost = Just post
                , movingPostPosition = post.position
              }
            , Cmd.none
            )

        DragEnd ->
            let
                movingPost =
                    Just model.movingPost
            in
                ( { model | movingPost = Nothing }
                , Cmd.none
                )

        DropOn post ->
            let
                section =
                    List.filter
                        (\s -> List.member post s.posts)
                        model.issue.sections
                        |> List.head
                        |> Maybe.andThen (\s -> Just(s))
            in
                ( { model
                    | droppedOnPost =
                        Just post
                    , sectionsToSave =
                        List.append model.sectionsToSave [ section ]
                  }
                , Cmd.none
                )

        DragOver post ->
            ( { model | draggedOverPost = Just post }
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
                            button [] [ text "Save" ]
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
    let
        isDroppable =
            model.movingPost /= Nothing

        isDragOverPost =
            model.draggedOverPost == Just post

        isMovingPost =
            model.movingPost == Just post

        baseTitle =
            post.title

        droppableTitle =
            if isDroppable then
                baseTitle ++ " droppable"
            else
                baseTitle

        draggableTitle =
            if isDragOverPost then
                droppableTitle ++ " dragover"
            else
                droppableTitle

        movingTitle =
            if isMovingPost then
                draggableTitle ++ " moving"
            else
                draggableTitle

        isDroppedOnPost =
            model.droppedOnPost == Just post

        title =
            if isDroppedOnPost then
                movingTitle ++ " dropped on"
            else
                movingTitle
    in
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
                    [ Block.text [] [ text title ] ]
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
