module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Random
import Svg exposing (Svg)
import Svg.Attributes as SA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { n : Int
    , m : Int
    , mappings : Array Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { n = 1
      , m = 1
      , mappings = Array.repeat 1 1
      }
    , Cmd.none
    )


type Msg
    = UpdateN String
    | UpdateM String
    | UpdateMapping Int String
    | GenerateRandom
    | NewRandomMapping (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateN nStr ->
            let
                newN =
                    clamp 0 maxSetSize <| Maybe.withDefault 0 (String.toInt nStr)
            in
            ( { model | n = newN, mappings = Array.repeat newN 1 }, Cmd.none )

        UpdateM mStr ->
            let
                newM =
                    clamp 0 maxSetSize <| Maybe.withDefault 0 (String.toInt mStr)
            in
            ( { model | m = newM }, Cmd.none )

        UpdateMapping index valueStr ->
            let
                value =
                    Maybe.withDefault 1 (String.toInt valueStr)
            in
            ( { model | mappings = Array.set index value model.mappings }, Cmd.none )

        GenerateRandom ->
            ( model
            , Random.generate NewRandomMapping (Random.list model.n (Random.int 1 model.m))
            )

        NewRandomMapping randomList ->
            ( { model | mappings = Array.fromList randomList }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Function Mapping App" ]
        , setSizeInput "n (domain size): " model.n UpdateN
        , setSizeInput "m (codomain size): " model.m UpdateM
        , Html.button [ E.onClick GenerateRandom ] [ Html.text "Random" ]
        , Html.div [] (List.indexedMap (viewMapping model) (Array.toList model.mappings))
        , viewSvgMapping model
        ]


setSizeInput : String -> Int -> (String -> Msg) -> Html Msg
setSizeInput label value toMsg =
    Html.div []
        [ Html.label [] [ Html.text label ]
        , Html.input
            [ A.type_ "number"
            , A.min "0"
            , A.max (String.fromInt maxSetSize)
            , A.value (String.fromInt value)
            , E.onInput toMsg
            ]
            []
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


viewSvgMapping : Model -> Html Msg
viewSvgMapping model =
    let
        svgWidth =
            (domainCodomainGridDist + 1) * gridUnit

        svgHeight =
            (2 + max model.n model.m) * gridUnit

        domainCircles =
            List.range 1 model.n
                |> List.map (\i -> viewCircle gridUnit (i * gridUnit) (String.fromInt i))

        codomainCircles =
            List.range 1 model.m
                |> List.map (\i -> viewCircle (domainCodomainGridDist * gridUnit) (i * gridUnit) (String.fromInt i))

        arrows =
            Array.toList model.mappings
                |> List.indexedMap (\i v -> viewArrow (i + 1) v)
    in
    Svg.svg
        [ SA.width (String.fromInt svgWidth)
        , SA.height (String.fromInt svgHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt svgWidth ++ " " ++ String.fromInt svgHeight)
        ]
        (Svg.defs []
            [ Svg.marker
                [ SA.id "arrowhead"
                , SA.markerWidth "10"
                , SA.markerHeight "7"
                , SA.refX "10"
                , SA.refY "3.5"
                , SA.orient "auto"
                ]
                [ Svg.polygon [ SA.points "0 0, 10 3.5, 0 7" ] [] ]
            ]
            :: domainCircles
            ++ codomainCircles
            ++ arrows
        )


viewCircle : Int -> Int -> String -> Svg msg
viewCircle x y label =
    Svg.g []
        [ Svg.circle
            [ SA.cx (String.fromInt x)
            , SA.cy (String.fromInt y)
            , SA.r (String.fromInt circleRadius)
            , SA.fill "white"
            , SA.stroke "black"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromInt x)
            , SA.y (String.fromInt y)
            , SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            ]
            [ Svg.text label ]
        ]


viewArrow : Int -> Int -> Svg msg
viewArrow from to =
    let
        x1 =
            gridUnit + circleRadius

        x2 =
            domainCodomainGridDist * gridUnit - circleRadius

        y1 =
            from * gridUnit

        y2 =
            to * gridUnit
    in
    Svg.line
        [ SA.x1 (String.fromInt x1)
        , SA.y1 (String.fromInt y1)
        , SA.x2 (String.fromInt x2)
        , SA.y2 (String.fromInt y2)
        , SA.stroke "black"
        , SA.markerEnd "url(#arrowhead)"
        ]
        []


maxSetSize : Int
maxSetSize =
    10


domainCodomainGridDist : Int
domainCodomainGridDist =
    5


circleRadius : Int
circleRadius =
    20


gridUnit : Int
gridUnit =
    50
