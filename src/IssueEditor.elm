module IssueEditor exposing (..)

import BasicAuth exposing (buildAuthorizationHeader)
import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Spacing as Spacing
import Debug exposing (log)
import DragAndDropEvents exposing (onDragStart, onDragOver, onDragEnd, onDrop)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Issue exposing (..)
import Post exposing (..)


-- import RemoteData exposing (WebData)

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

    -- only when we're dragging a Post:
    , currentSection : Maybe Section.Model
    , movingPost : Maybe Post.Model
    , draggedOverPost : Maybe Post.Model
    , droppedOnPost : Maybe Post.Model

    -- only when we're dragging a section:
    , movingSection : Maybe Section.Model
    , draggedOverSection : Maybe Section.Model
    , droppedOnSection : Maybe Section.Model

    --
    , accordionState : Accordion.State
    , loadingError : String
    , sectionsToSave : List Section.Model
    , postsToSave : List Post.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { issue = Issue.dummyIssue -- <-- smells to me
      , loadingError = ""
      , accordionState = Accordion.initialState
      , movingPost = Nothing
      , draggedOverPost = Nothing
      , droppedOnPost = Nothing
      , movingSection = Nothing
      , draggedOverSection = Nothing
      , droppedOnSection = Nothing
      , currentSection = Nothing
      , sectionsToSave = []
      , postsToSave = []
      }
    , (getIssue 472)
    )



-- Update


type Msg
    = PostDragStart Post.Model
    | PostDragEnd
    | PostDropOn Post.Model
    | PostDragOver Post.Model
    | AccordionMsg Accordion.State
    | LoadIssue (Result Http.Error Issue.Model)
    | SectionDragStart Section.Model
    | SectionDragEnd
    | SectionDropOn Section.Model
    | SectionDragOver Section.Model
    | SaveSection Section.Model
    | SaveSectionPosts Section.Model
    | OnPostSave (Result Http.Error Post.Model)
    | OnSectionSave (Result Http.Error Section.Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostDragStart post ->
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

        PostDragEnd ->
            ( { model
                | movingPost = Nothing
                , draggedOverPost = Nothing
                , currentSection = Nothing
              }
            , Cmd.none
            )

        PostDropOn post ->
            let
                droppedOnModel =
                    moveMovingPost model post
            in
                ( { droppedOnModel
                    | droppedOnPost = Just post
                  }
                , Cmd.none
                )

        PostDragOver post ->
            ( { model
                | draggedOverPost = Just post
              }
            , Cmd.none
            )

        SectionDragStart section ->
            ( { model
                | movingSection = Just section
              }
            , Cmd.none
            )

        SectionDragEnd ->
            ( { model
                | movingSection = Nothing
                , draggedOverSection = Nothing
              }
            , Cmd.none
            )

        SectionDropOn section ->
            let
                droppedOnModel =
                    moveMovingSection model section
            in
                ( { droppedOnModel
                    | droppedOnSection = Just section
                  }
                , Cmd.none
                )

        SectionDragOver section ->
            ( { model
                | draggedOverSection = Just section
              }
            , Cmd.none
            )

        SaveSection section ->
            ( { model
                | sectionsToSave =
                    List.filter
                        (\s -> s.id /= section.id)
                        model.sectionsToSave
              }
            , saveSectionCmd section
            )

        SaveSectionPosts section ->
            let
                postCmds =
                    List.map (\p -> savePostCmd p) section.posts
            in
                ( { model | postsToSave = section.posts }
                , Cmd.batch postCmds
                )

        LoadIssue (Ok issue) ->
            ( { model | issue = issue }, Cmd.none )

        LoadIssue (Err msg) ->
            ( { model
                | loadingError = toString msg
              }
            , Cmd.none
            )

        OnPostSave (Ok post) ->
            let
                newPostsToSave =
                    List.filter (\post -> post.id /= post.id) model.postsToSave
            in
                ( { model | postsToSave = newPostsToSave }, Cmd.none )

        OnPostSave (Err msg) ->
            ( { model | loadingError = toString msg }, Cmd.none )

        OnSectionSave (Ok section) ->
            let
                otherSections =
                    List.filter (\s -> s.id /= section.id) model.issue.sections

                issue =
                    model.issue
            in
                ( { model | issue = { issue | sections = otherSections ++ [ section ] } }
                , Cmd.none
                )

        OnSectionSave (Err msg) ->
            ( { model | loadingError = toString msg }, Cmd.none )

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
    let
        modifiedModel =
            renumberPosts
                (insertMovingPost
                    (removeMovingPost model)
                    here
                )

        movingPost =
            case model.movingPost of
                Nothing ->
                    Debug.crash "model.movingPost is Nothing"

                Just movingPost ->
                    movingPost

        sectionToSave =
            getSectionForPost model movingPost

        sectionsToSave =
            if List.member sectionToSave.id (List.map .id model.sectionsToSave) then
                model.sectionsToSave
            else
                model.sectionsToSave ++ [ sectionToSave ]
    in
        { modifiedModel
            | sectionsToSave = sectionsToSave
        }


removeMovingPost : Model -> Model
removeMovingPost model =
    let
        currentSection =
            case model.currentSection of
                Nothing ->
                    Debug.crash "model.currentSection is Nothing"

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
                    (List.filter (\s -> s.id /= currentSection.id) issue.sections)
                        ++ [ newSection ]
            }
    in
        { model
            | currentSection = Just newSection
            , issue = newIssue
        }



-- Section functions


moveMovingSection : Model -> Section.Model -> Model
moveMovingSection model here =
    renumberSections (insertMovingSection (removeMovingSection model) here)


removeMovingSection : Model -> Model
removeMovingSection model =
    let
        movingSection =
            case model.movingSection of
                Nothing ->
                    Debug.crash "asdfasdf"

                Just movingSection ->
                    movingSection

        issue =
            model.issue

        newIssue =
            { issue
                | sections =
                    (List.filter (\s -> s /= movingSection) issue.sections)
            }
    in
        { model
            | issue = newIssue
        }


insertMovingSection : Model -> Section.Model -> Model
insertMovingSection model here =
    let
        head =
            List.take (here.position - 1) model.issue.sections

        tail =
            List.drop (here.position - 1) model.issue.sections

        movingSection =
            case model.movingSection of
                Nothing ->
                    Debug.crash "what a joke"

                Just movingSection ->
                    movingSection

        isNotMovingSection section =
            section /= movingSection

        headless =
            List.filter isNotMovingSection head

        tailless =
            List.filter isNotMovingSection tail

        issue =
            model.issue

        newIssue =
            { issue
                | sections = headless ++ [ movingSection ] ++ tailless
            }
    in
        { model
            | issue = newIssue
        }


renumberSections : Model -> Model
renumberSections model =
    let
        renumber idx section =
            { section | position = idx + 1 }

        issue =
            model.issue

        newIssue =
            { issue
                | sections = List.indexedMap renumber model.issue.sections
            }
    in
        { model
            | issue = newIssue
        }



-- View


view : Model -> Html Msg
view model =
    let
        sortedSections =
            List.sortBy .position model.issue.sections
    in
        if not (String.isEmpty model.loadingError) then
            text model.loadingError
        else
            Grid.container []
                [ CDN.stylesheet
                , Grid.row []
                    [ Grid.col []
                        [ Accordion.config AccordionMsg
                            |> Accordion.withAnimation
                            |> Accordion.cards
                                (List.map (sectionView model) sortedSections)
                            |> Accordion.view model.accordionState
                        ]
                    ]
                ]


sectionView : Model -> Section.Model -> Accordion.Card Msg
sectionView model section =
    let
        baseAttrs =
            [ attribute "draggable" "true" ]

        attrs =
            case model.movingPost of
                Nothing ->
                    baseAttrs
                        ++ [ onDragOver <| SectionDragOver section
                           , onDragStart <| SectionDragStart section
                           , onDragEnd <| SectionDragEnd
                           , onDrop <| SectionDropOn section
                           ]

                Just movingPost ->
                    baseAttrs

        showSaveButton =
            List.member section.id (List.map .id model.sectionsToSave)
    in
        Accordion.card
            { id = toString section.id
            , options =
                [ Card.attrs attrs ]
            , header =
                Accordion.header [] <|
                    Accordion.toggle []
                        [ text
                            section.name
                        , if showSaveButton then
                            Button.button
                                [ Button.warning
                                , Button.small
                                , Button.attrs
                                    [ onClick (SaveSectionPosts section)
                                    , Spacing.ml2
                                    ]
                                ]
                                [ text "Save Post Order" ]
                          else
                            text ""
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
                , onDragOver <| PostDragOver post
                , onDragStart <| PostDragStart post
                , onDragEnd <| PostDragEnd
                , onDrop <| PostDropOn post
                ]
            ]
            |> Card.block []
                [ Block.text []
                    [ text post.title ]
                ]
            |> Card.view
        )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Accordion.subscriptions model.accordionState AccordionMsg



-- HTTP


baseUrl : String
baseUrl =
    -- "https://aashe-bulletin-stage.herokuapp.com/api"
    "http://127.0.0.1:7000/api"


getIssue : Int -> Cmd Msg
getIssue id =
    let
        url =
            baseUrl ++ "/issue/" ++ toString id
    in
        Http.send LoadIssue (Http.get url decodeIssue)


savePostUrl : Int -> String
savePostUrl postId =
    baseUrl ++ "/post/" ++ toString postId


savePostRequest : Post.Model -> Http.Request Post.Model
savePostRequest post =
    Http.request
        { body = Post.encodePost post |> Http.jsonBody
        , expect = Http.expectJson Post.decodePost
        , headers =
            [ buildAuthorizationHeader "superman" "krypton" ]
        , method = "PUT"
        , timeout = Nothing
        , url = savePostUrl post.id
        , withCredentials = True
        }


savePostCmd : Post.Model -> Cmd Msg
savePostCmd post =
    savePostRequest post
        |> Http.send OnPostSave


saveSectionUrl : Int -> String
saveSectionUrl sectionId =
    baseUrl ++ "/section/" ++ toString sectionId


saveSectionRequest : Section.Model -> Http.Request Section.Model
saveSectionRequest section =
    Http.request
        { body = Section.encodeSection section |> Http.jsonBody
        , expect = Http.expectJson Section.decodeSection
        , headers =
            [ buildAuthorizationHeader "superman" "krypton" ]
        , method = "PUT"
        , timeout = Nothing
        , url = saveSectionUrl section.id
        , withCredentials = False
        }


saveSectionCmd : Section.Model -> Cmd Msg
saveSectionCmd section =
    saveSectionRequest section
        |> Http.send OnSectionSave
