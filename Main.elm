module Main exposing (main)

import Debug exposing (crash, log)
import DragAndDropEvents exposing (onDragStart, onDragOver, onDragEnd, onDrop)
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }



-- Model


type alias Post =
    { title : String
    , position : Int
    }


type alias Model =
    { posts : List Post
    , movingPost : Maybe Post
    , draggedOverPost : Maybe Post
    , droppedOnPost : Maybe Post
    }


init : ( Model, Cmd Msg )
init =
    ( { posts =
            [ { title = "Post One"
              , position = 1
              }
            , { title = "Post Two"
              , position = 2
              }
            , { title = "Post Three"
              , position = 3
              }
            , { title = "Post Four"
              , position = 4
              }
            ]
      , movingPost = Nothing
      , draggedOverPost = Nothing
      , droppedOnPost = Nothing
      }
    , Cmd.none
    )



-- Update


type Msg
    = DragStart Post
    | DragEnd
    | DropOn Post
    | DragOver Post


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
            ( { model | draggedOverPost = Just post }
            , Cmd.none
            )


moveMovingPost : Model -> Post -> Model
moveMovingPost model beforeThis =
    repositionPosts
        (insertMovingPost (removeMovingPost model) beforeThis)


repositionPosts : Model -> Model
repositionPosts model =
    let
        renumber idx post =
            { post | position = idx }
    in
        { model
            | posts = List.indexedMap renumber model.posts
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
    in
        { model
            | posts = List.filter (\p -> p /= removeThis) model.posts
        }


insertMovingPost : Model -> Post -> Model
insertMovingPost model beforeThis =
    let
        head =
            List.take (beforeThis.position - 1) model.posts

        tail =
            List.drop (beforeThis.position - 1) model.posts

        movingPost =
            model.movingPost

        insertThis =
            case model.movingPost of
                Nothing ->
                    Debug.crash ("stupid")

                Just movingPost ->
                    movingPost
    in
        { model
            | posts = head ++ [ insertThis ] ++ tail
        }



-- View


view : Model -> Html Msg
view model =
    div [] (viewPosts model)


viewPosts : Model -> List (Html Msg)
viewPosts model =
    let
        sortedPosts =
            List.sortBy .position model.posts
    in
        List.map viewPost sortedPosts


viewPost : Post -> Html Msg
viewPost post =
    div
        [ attribute "draggable" "true"
        , onDragOver <| DragOver post
        , onDragStart <| DragStart post
        , onDragEnd <| DragEnd
        , onDrop <| DropOn post
        ]
        [ h3 []
            [ text (toString post.position)
            , text ".)  "
            , text post.title
            ]
        ]
