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


type alias Item =
    { title : String
    , position : Int
    }


type alias Model =
    { items : List Item
    , movingItem : Maybe Item
    , draggedOverItem : Maybe Item
    , droppedOnItem : Maybe Item
    }


init : ( Model, Cmd Msg )
init =
    ( { items =
            [ { title = "Item One"
              , position = 1
              }
            , { title = "Item Two"
              , position = 2
              }
            , { title = "Item Three"
              , position = 3
              }
            , { title = "Item Four"
              , position = 4
              }
            ]
      , movingItem = Nothing
      , draggedOverItem = Nothing
      , droppedOnItem = Nothing
      }
    , Cmd.none
    )



-- Update


type Msg
    = DragStart Item
    | DragEnd
    | DropOn Item
    | DragOver Item


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart item ->
            ( { model
                | movingItem = Just item
              }
            , Cmd.none
            )

        DragEnd ->
            ( { model
                | movingItem = Nothing
              }
            , Cmd.none
            )

        DropOn item ->
            let
                droppedOnModel =
                    moveMovingItem model item
            in
                ( { droppedOnModel
                    | droppedOnItem = Just item
                  }
                , Cmd.none
                )

        DragOver item ->
            ( { model | draggedOverItem = Just item }
            , Cmd.none
            )


moveMovingItem : Model -> Item -> Model
moveMovingItem model overThis =
    repositionItems
        (insertMovingItem (removeMovingItem model) overThis)


repositionItems : Model -> Model
repositionItems model =
    let
        renumber idx item =
            { item | position = idx + 1 }
    in
        { model
            | items = List.indexedMap renumber model.items
        }


removeMovingItem : Model -> Model
removeMovingItem model =
    let
        removeThis =
            case model.movingItem of
                Nothing ->
                    Debug.crash ("stupid")

                Just movingItem ->
                    movingItem
    in
        { model
            | items = List.filter (\p -> p /= removeThis) model.items
        }


insertMovingItem : Model -> Item -> Model
insertMovingItem model overThis =
    let
        movingItem =
            model.movingItem

        actualMovingItem =
            case movingItem of
                Nothing ->
                    Debug.crash "what a joke"

                Just movingItem ->
                    movingItem

        head =
            List.take (overThis.position - 1) model.items

        headless =
            List.filter (\p -> p /= actualMovingItem) head

        tail =
            List.drop (overThis.position - 1) model.items

        tailless =
            List.filter (\p -> p /= actualMovingItem) tail
    in
        { model
            | items = headless ++ [ actualMovingItem ] ++ tailless
        }



-- View


view : Model -> Html Msg
view model =
    div [] (viewItems model)


viewItems : Model -> List (Html Msg)
viewItems model =
    let
        sortedItems =
            List.sortBy .position model.items
    in
        List.map (\i -> viewItem model i) sortedItems


viewItem : Model -> Item -> Html Msg
viewItem model item =
    let
        label =
            (toString item.position)
                ++ ".) "
                ++ item.title
    in
        div
            [ attribute "draggable" "true"
            , onDragOver <| DragOver item
            , onDragStart <| DragStart item
            , onDragEnd <| DragEnd
            , onDrop <| DropOn item
            ]
            [ h3 []
                [ text label
                ]
            ]
