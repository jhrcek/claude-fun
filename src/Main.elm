module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { n : Int
    , m : Int
    , mappings : Array Int
    }


init : Model
init =
    { n = 0
    , m = 0
    , mappings = Array.empty
    }


type Msg
    = UpdateN String
    | UpdateM String
    | UpdateMapping Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateN nStr ->
            let
                newN =
                    Maybe.withDefault 0 (String.toInt nStr)
            in
            { model | n = newN, mappings = Array.repeat newN 1 }

        UpdateM mStr ->
            let
                newM =
                    Maybe.withDefault 0 (String.toInt mStr)
            in
            { model | m = newM }

        UpdateMapping index valueStr ->
            let
                value =
                    Maybe.withDefault 1 (String.toInt valueStr)
            in
            { model | mappings = Array.set index value model.mappings }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Function Mapping App" ]
        , Html.div []
            [ Html.label [] [ Html.text "n (domain size): " ]
            , Html.input [ A.type_ "number", A.value (String.fromInt model.n), E.onInput UpdateN ] []
            ]
        , Html.div []
            [ Html.label [] [ Html.text "m (codomain size): " ]
            , Html.input [ A.type_ "number", A.value (String.fromInt model.m), E.onInput UpdateM ] []
            ]
        , Html.div [] (List.indexedMap (viewMapping model) (Array.toList model.mappings))
        ]


viewMapping : Model -> Int -> Int -> Html Msg
viewMapping model index value =
    Html.div []
        [ Html.text ("f(" ++ String.fromInt (index + 1) ++ ") = ")
        , Html.select [ E.onInput (UpdateMapping index) ]
            (List.map (viewOption value) (List.range 1 model.m))
        ]


viewOption : Int -> Int -> Html msg
viewOption selected value =
    Html.option [ A.value (String.fromInt value), A.selected (value == selected) ]
        [ Html.text (String.fromInt value) ]
