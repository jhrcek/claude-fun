module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Random
import Random.List
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
    { domain : Int
    , codomain : Int
    , mappings : Array Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { domain = 1
      , codomain = 1
      , mappings = Array.repeat 1 0
      }
    , Cmd.none
    )


type Msg
    = UpdateDomain String
    | UpdateCodomain String
    | UpdateMapping Int String
    | GenFunction
    | GenInjective
    | GenSurjective
    | GenBijective
    | NewRandomMapping (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDomain domStr ->
            let
                newN =
                    clamp 0 maxSetSize <| Maybe.withDefault 0 (String.toInt domStr)
            in
            ( { model
                | domain = newN
                , mappings =
                    Array.initialize newN (\x -> Array.get x model.mappings |> Maybe.withDefault 0)
              }
            , Cmd.none
            )

        UpdateCodomain codStr ->
            let
                newM =
                    clamp 0 maxSetSize <| Maybe.withDefault 0 (String.toInt codStr)
            in
            ( { model
                | codomain = newM
                , mappings =
                    Array.map
                        (\y ->
                            if y > newM then
                                0

                            else
                                y
                        )
                        model.mappings
              }
            , Cmd.none
            )

        UpdateMapping index valueStr ->
            let
                value =
                    case String.toInt valueStr of
                        Just v ->
                            v

                        Nothing ->
                            0
            in
            ( { model | mappings = Array.set index value model.mappings }, Cmd.none )

        GenFunction ->
            ( model
            , Random.generate NewRandomMapping (Random.list model.domain (Random.int 1 model.codomain))
            )

        GenInjective ->
            ( model
            , Random.generate NewRandomMapping (genInjective model.domain model.codomain)
            )

        GenSurjective ->
            ( model
            , Random.generate NewRandomMapping (genSurjective model.domain model.codomain)
            )

        GenBijective ->
            ( model
            , Random.generate NewRandomMapping (genBijective model.domain)
            )

        NewRandomMapping randomList ->
            ( { model | mappings = Array.fromList randomList }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Function Mapping App" ]
        , setSizeInput "Domain size: " model.domain UpdateDomain
        , setSizeInput "Codomain size: " model.codomain UpdateCodomain
        , Html.text "Generate function: "
        , Html.button [ E.onClick GenFunction ] [ Html.text "Any" ]
        , Html.button
            [ E.onClick GenInjective
            , A.disabled (model.domain > model.codomain)
            ]
            [ Html.text "Injective" ]
        , Html.button
            [ E.onClick GenSurjective
            , A.disabled (model.domain < model.codomain)
            ]
            [ Html.text "Surjective" ]
        , Html.button
            [ E.onClick GenBijective
            , A.disabled (model.domain /= model.codomain)
            ]
            [ Html.text "Bijective" ]
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
            (viewOption 0 "?" :: List.map (viewOption value << String.fromInt) (List.range 1 model.codomain))
        ]


viewOption : Int -> String -> Html msg
viewOption selected value =
    Html.option
        [ A.value value
        , A.selected (String.fromInt selected == value)
        ]
        [ Html.text value ]


viewSvgMapping : Model -> Html Msg
viewSvgMapping model =
    let
        svgWidth =
            (domainCodomainGridDist + 1) * gridUnit

        svgHeight =
            (2 + max model.domain model.codomain) * gridUnit

        domainCircles =
            List.range 1 model.domain
                |> List.map (\i -> viewCircle (Array.get (i - 1) model.mappings == Just 0) 1 i)

        codomainCircles =
            List.range 1 model.codomain
                |> List.map (\i -> viewCircle False domainCodomainGridDist i)

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


viewCircle : Bool -> Int -> Int -> Svg msg
viewCircle isHighlighted gridX gridY =
    Svg.g []
        [ Svg.circle
            [ SA.cx <| String.fromInt <| gridX * gridUnit
            , SA.cy <| String.fromInt <| gridY * gridUnit
            , SA.r (String.fromInt circleRadius)
            , SA.fill "white"
            , SA.strokeWidth "1"
            , SA.stroke <|
                if isHighlighted then
                    "red"

                else
                    "black"
            ]
            []
        , Svg.text_
            [ SA.x <| String.fromInt <| gridX * gridUnit
            , SA.y <| String.fromInt <| gridY * gridUnit
            , SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            ]
            [ Svg.text (String.fromInt gridY) ]
        ]


viewArrow : Int -> Int -> Svg msg
viewArrow from to =
    if to == 0 then
        -- Don't draw an arrow for unspecified mappings
        Svg.g [] []

    else
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


genInjective : Int -> Int -> Random.Generator (List Int)
genInjective domain codomain =
    List.range 1 codomain
        |> Random.List.shuffle
        |> Random.map (List.take domain)


genBijective : Int -> Random.Generator (List Int)
genBijective domain =
    List.range 1 domain |> Random.List.shuffle


genSurjective : Int -> Int -> Random.Generator (List Int)
genSurjective domain codomain =
    List.range 1 domain
        |> Random.List.shuffle
        |> Random.andThen
            (\shuffled ->
                let
                    -- ensure that each codomain element has at least 1 element from the domain mapping to it
                    startMappings =
                        List.map2 Tuple.pair (List.take codomain shuffled) (List.range 1 codomain)
                in
                -- assign remaining domain elements randomly to 1..codomain
                Random.list (domain - codomain) (Random.int 1 codomain)
                    |> Random.map
                        (\cod ->
                            startMappings
                                ++ List.map2 Tuple.pair (List.drop codomain shuffled) cod
                                |> List.sortBy Tuple.first
                                |> List.map Tuple.second
                        )
            )
