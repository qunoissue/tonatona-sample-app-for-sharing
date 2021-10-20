module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Attributes
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ on "click" (Decode.succeed Decrement), on "click" (Decode.succeed Increment) ] [ text "+1" ]
            , div [] [ text <| String.fromInt model.count ]
            , button [ onClick Decrement ] [ text "-1" ]
            ]
        , case deepTree of
            Ok tree ->
                deepTreeView tree
            Err _ ->
                text ""
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


maxDepth: Int
maxDepth = 20

deepJson : Int -> Value
deepJson num =
    Encode.object
        [ ("child"
          , if num < maxDepth then
                deepJson (num + 1)
            else
                Encode.null
          )
        , ("depth", Encode.int num)
        ]


type Tree
    = Tree Tree_


type alias Tree_ =
    { child : Maybe Tree
    , depth : Int
    }


deepJsonDecoder : Decoder Tree
deepJsonDecoder =
    Decode.map2 Tree_
        ( Decode.field "child"
            (Decode.maybe
                (Decode.lazy
                    (\_ -> deepJsonDecoder)
                )
            )
        )
        ( Decode.field "depth" Decode.int)
        |> Decode.map Tree

deepTree : Result Decode.Error Tree
deepTree
    = Decode.decodeValue deepJsonDecoder (deepJson 0)


deepTreeView : Tree -> Html msg
deepTreeView (Tree tree) =
    div [ Attributes.style "padding" "10px" ]
        [ text <| "depth: " ++ String.fromInt tree.depth
        , case tree.child of
            Just tree_ ->
                deepTreeView tree_
            Nothing ->
                text ""
        ]
