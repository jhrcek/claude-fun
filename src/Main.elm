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
    { f : Fun
    , g : Fun
    , commonCodomain : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { f = { domain = 1, mappings = Array.repeat 1 0 }
      , g = { domain = 1, mappings = Array.repeat 1 0 }
      , commonCodomain = 1
      }
    , Cmd.none
    )


type Function
    = F
    | G


type alias Fun =
    { domain : Int, mappings : Array Int }


overMappings : (Array Int -> Array Int) -> Fun -> Fun
overMappings f fun =
    { fun | mappings = f fun.mappings }


overFun : Function -> (Fun -> Fun) -> Model -> Model
overFun fun f model =
    case fun of
        F ->
            { model | f = f model.f }

        G ->
            { model | g = f model.g }


type Msg
    = SetDomainSize Function Int
    | SetCommonCodomainSize Int
    | UpdateMapping Function Int String
    | GenFunction Function FunctionProperty
    | NewRandomMapping Function (List Int)
    | NoOp


type FunctionProperty
    = Any
    | Injective
    | Surjective
    | Bijective
    | Monotonic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDomainSize fun newDomain ->
            pure <|
                overFun fun
                    (\fn ->
                        { fn
                            | domain = newDomain
                            , mappings = Array.initialize newDomain (\x -> Array.get x fn.mappings |> Maybe.withDefault 0)
                        }
                    )
                    model

        SetCommonCodomainSize newCodomainSize ->
            let
                restrictToNewCodomain y =
                    if y > newCodomainSize then
                        -- if the codomain size is reduced, set mappings to no longer existing codomains to "undefined"
                        0

                    else
                        y
            in
            pure
                { model
                    | commonCodomain = newCodomainSize
                    , f = overMappings (Array.map restrictToNewCodomain) model.f
                    , g = overMappings (Array.map restrictToNewCodomain) model.g
                }

        UpdateMapping fun index valueStr ->
            let
                value =
                    String.toInt valueStr |> Maybe.withDefault 0
            in
            pure <| overFun fun (overMappings (Array.set index value)) model

        GenFunction fun prop ->
            ( model
            , Random.generate (NewRandomMapping fun) <|
                case prop of
                    Any ->
                        Random.list (getDomain fun model) (Random.int 1 model.commonCodomain)

                    Injective ->
                        genInjective (getDomain fun model) model.commonCodomain

                    Surjective ->
                        genSurjective (getDomain fun model) model.commonCodomain

                    Bijective ->
                        genBijective (getDomain fun model)

                    Monotonic ->
                        Random.map List.sort <| Random.list (getDomain fun model) (Random.int 1 model.commonCodomain)
            )

        NewRandomMapping fun randomList ->
            pure <| overFun fun (overMappings (\_ -> Array.fromList randomList)) model

        NoOp ->
            pure model


pure : Model -> ( Model, Cmd msg )
pure model =
    ( model, Cmd.none )


getDomain : Function -> Model -> Int
getDomain f m =
    case f of
        F ->
            m.f.domain

        G ->
            m.g.domain


view : Model -> Html Msg
view model =
    let
        genButtons fun =
            let
                dom =
                    getDomain fun model
            in
            [ Html.text ("Generate " ++ functionName fun ++ ": ")
            , Html.button [ E.onClick (GenFunction fun Any) ] [ Html.text "Any" ]
            , Html.button
                [ E.onClick (GenFunction fun Injective)
                , A.disabled (dom > model.commonCodomain)
                , A.title <|
                    if dom > model.commonCodomain then
                        "There are no injective functions when the domain is larger than the codomain"

                    else
                        "Generate random injective function"
                ]
                [ Html.text "Injective" ]
            , Html.button
                [ E.onClick (GenFunction fun Surjective)
                , A.disabled (dom < model.commonCodomain)
                , A.title <|
                    if dom < model.commonCodomain then
                        "There are no surjective functions when the domain is smaller than the codomain"

                    else
                        "Generate random surjective function"
                ]
                [ Html.text "Surjective" ]
            , Html.button
                [ E.onClick (GenFunction fun Bijective)
                , A.disabled (dom /= model.commonCodomain)
                , A.title <|
                    if dom /= model.commonCodomain then
                        "Bijective functions only exist when the domain and codomain have the same size"

                    else
                        "Generate random bijective function"
                ]
                [ Html.text "Bijective" ]
            , Html.button
                [ E.onClick (GenFunction fun Monotonic) ]
                [ Html.text "Monotonic" ]
            ]
    in
    Html.div []
        [ Html.h1 [] [ Html.text "Pullbacks in Set" ]
        , setSizeInput "f domain size: " model.f.domain (SetDomainSize F)
        , setSizeInput "g domain size: " model.g.domain (SetDomainSize G)
        , setSizeInput "Codomain size: " model.commonCodomain SetCommonCodomainSize
        , Html.div [] (genButtons F)
        , Html.div [] (genButtons G)
        , Html.div [ A.style "display" "grid", A.style "grid-template-columns" "100px 100px" ]
            [ Html.div [] (List.indexedMap (viewMapping F model) (Array.toList model.f.mappings))
            , Html.div [] (List.indexedMap (viewMapping G model) (Array.toList model.g.mappings))
            ]
        , viewPullback model
        ]


functionName : Function -> String
functionName fun =
    case fun of
        F ->
            "f"

        G ->
            "g"


setSizeInput : String -> Int -> (Int -> Msg) -> Html Msg
setSizeInput label value toMsg =
    Html.div []
        [ Html.label [] [ Html.text label ]
        , Html.input
            [ A.type_ "number"
            , A.min "0"
            , A.max (String.fromInt maxSetSize)
            , A.value (String.fromInt value)
            , E.onInput
                (\str ->
                    case String.toInt str of
                        Just i ->
                            toMsg (clamp minSetSize maxSetSize i)

                        Nothing ->
                            NoOp
                )
            ]
            []
        ]


viewMapping : Function -> Model -> Int -> Int -> Html Msg
viewMapping fun model index value =
    Html.div []
        [ Html.text (functionName fun ++ "(" ++ String.fromInt (index + 1) ++ ") = ")
        , Html.select [ E.onInput (UpdateMapping fun index) ]
            (viewOption 0 "?" :: List.map (viewOption value << String.fromInt) (List.range 1 model.commonCodomain))
        ]


viewOption : Int -> String -> Html msg
viewOption selected value =
    Html.option
        [ A.value value
        , A.selected (String.fromInt selected == value)
        ]
        [ Html.text value ]


viewPullback : Model -> Html Msg
viewPullback model =
    let
        gridWidth =
            2 + model.f.domain + max model.g.domain model.commonCodomain

        svgWidth =
            gridWidth * gridUnit

        gridHeight =
            2 + model.g.domain + max model.f.domain model.commonCodomain

        svgHeight =
            gridHeight * gridUnit

        fDomainCircles =
            List.range 1 model.f.domain
                |> List.map (\i -> viewCircle (String.fromInt i) (Array.get (i - 1) model.f.mappings == Just 0) (model.f.domain - i + 1) (model.g.domain + i + 1))

        gDomainCircles =
            List.range 1 model.g.domain
                |> List.map (\i -> viewCircle (String.fromInt i) (Array.get (i - 1) model.g.mappings == Just 0) (model.f.domain + i + 1) (model.g.domain - i + 1))

        codomainCircles =
            List.range 1 model.commonCodomain
                |> List.map (\i -> viewCircle (String.fromInt i) False (model.f.domain + i + 1) (model.g.domain + i + 1))

        fArrows =
            Array.toList model.f.mappings
                |> List.indexedMap
                    (\i v ->
                        viewArrowGrid
                            -- horizontal
                            True
                            (model.f.domain - i)
                            (model.g.domain + (i + 1 + 1))
                            (model.f.domain + v + 1)
                            (model.g.domain + v + 1)
                            v
                    )

        gArrows =
            Array.toList model.g.mappings
                |> List.indexedMap
                    (\i v ->
                        viewArrowGrid
                            --vertical
                            False
                            (model.f.domain + i + 2)
                            (model.g.domain - i)
                            (model.f.domain + v + 1)
                            (model.g.domain + v + 1)
                            v
                    )

        pullbackCircles =
            List.range 1 model.f.domain
                |> List.concatMap
                    (\x1 ->
                        List.range 1 model.g.domain
                            |> List.concatMap
                                (\x2 ->
                                    case ( Array.get (x1 - 1) model.f.mappings, Array.get (x2 - 1) model.g.mappings ) of
                                        ( Just fx, Just gx ) ->
                                            if fx == gx && fx /= 0 then
                                                [ viewCircle (String.fromInt x1 ++ "," ++ String.fromInt x2)
                                                    False
                                                    (model.f.domain - x1 + 1)
                                                    (model.g.domain - x2 + 1)
                                                ]

                                            else
                                                []

                                        _ ->
                                            []
                                )
                    )
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
            :: viewGrid gridWidth gridHeight
            :: fDomainCircles
            ++ gDomainCircles
            ++ codomainCircles
            ++ fArrows
            ++ gArrows
            ++ pullbackCircles
        )


viewCircle : String -> Bool -> Int -> Int -> Svg msg
viewCircle label isHighlighted gridX gridY =
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
            [ Svg.text label ]
        ]


viewArrowGrid : Bool -> Int -> Int -> Int -> Int -> Int -> Svg msg
viewArrowGrid isHoriz fromGridX fromGridY toGridX toGridY val =
    if val == 0 then
        -- Don't draw an arrow for unspecified mappings
        Svg.g [] []

    else
        let
            hr =
                -- horizontal radius
                if isHoriz then
                    circleRadius

                else
                    0

            vr =
                -- vertical radius
                if isHoriz then
                    0

                else
                    circleRadius

            x1 =
                fromGridX * gridUnit + hr

            x2 =
                toGridX * gridUnit - hr

            y1 =
                fromGridY * gridUnit + vr

            y2 =
                toGridY * gridUnit - vr
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


viewGrid : Int -> Int -> Svg msg
viewGrid width height =
    let
        horizontalLines =
            List.range 0 height
                |> List.map
                    (\i ->
                        Svg.line
                            [ SA.x1 "0"
                            , SA.y1 (String.fromInt (i * gridUnit))
                            , SA.x2 (String.fromInt (width * gridUnit))
                            , SA.y2 (String.fromInt (i * gridUnit))
                            , SA.stroke "#eee"
                            , SA.strokeWidth "1"
                            ]
                            []
                    )

        verticalLines =
            List.range 0 width
                |> List.map
                    (\i ->
                        Svg.line
                            [ SA.x1 (String.fromInt (i * gridUnit))
                            , SA.y1 "0"
                            , SA.x2 (String.fromInt (i * gridUnit))
                            , SA.y2 (String.fromInt (height * gridUnit))
                            , SA.stroke "#eee"
                            , SA.strokeWidth "1"
                            ]
                            []
                    )
    in
    Svg.g [] (horizontalLines ++ verticalLines)


minSetSize : Int
minSetSize =
    1


maxSetSize : Int
maxSetSize =
    10


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
