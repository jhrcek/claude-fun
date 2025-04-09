module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Dict
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


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
    , dragState : Maybe DragState
    }


type alias DragState =
    { domainElement : Int
    , currentPosition : Maybe ( Int, Int )
    }


{-| Function without any undefined mappings (0s within the array)
-}
type TotalFunction
    = TotalFunction
        { domain : Int
        , codomain : Int
        , mappings : Array Int
        }


mkTotal :
    { r
        | domain : Int
        , codomain : Int
        , mappings : Array Int
    }
    -> Maybe TotalFunction
mkTotal r =
    if Array.Extra.any (\e -> e == 0) r.mappings then
        Nothing

    else
        Just
            (TotalFunction
                { domain = r.domain
                , codomain = r.codomain
                , mappings = r.mappings
                }
            )


{-| compose f g == g ∘ f
-}
compose : TotalFunction -> TotalFunction -> TotalFunction
compose (TotalFunction f) (TotalFunction g) =
    TotalFunction
        { domain = f.domain
        , codomain = g.codomain
        , mappings =
            Array.map
                (\i ->
                    Array.get (i - 1) g.mappings
                        |> Maybe.andThen (\fi -> Array.get (fi - 1) f.mappings)
                        |> Maybe.withDefault 0
                )
                f.mappings
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        domain =
            3
    in
    ( { domain = domain
      , codomain = 4
      , mappings = Array.repeat domain 0
      , dragState = Nothing
      }
    , Cmd.none
    )


type Msg
    = UpdateDomain String
    | UpdateCodomain String
    | UpdateMapping Int String
    | Generate FunctionProperty
    | NewRandomMapping (List Int)
    | DragStart Int
    | DragOver Int Int
    | DragEnd Int Int
    | RemoveMapping Int


type FunctionProperty
    = Any
    | Injective
    | Surjective
    | Bijective
    | Idempotent


type alias PropertyConfig =
    { label : String
    , isDisabled : Int -> Int -> Bool
    , disabledExplanation : Maybe String -- TODO making impossible states impossible this should be Just <=> isDisabled
    , count : Int -> Int -> Int
    , generator : Int -> Int -> Random.Generator (List Int)
    }


propertyConfig : FunctionProperty -> PropertyConfig
propertyConfig prop =
    case prop of
        Any ->
            { label = "Any"
            , isDisabled = \_ _ -> False
            , disabledExplanation = Nothing
            , count = countAllFunctions
            , generator = genAny
            }

        Injective ->
            { label = "Injective"
            , isDisabled = \domain codomain -> domain > codomain
            , disabledExplanation = Just "There are no injective functions when the domain is larger than the codomain"
            , count = countInjective
            , generator = genInjective
            }

        Surjective ->
            { label = "Surjective"
            , isDisabled = \domain codomain -> domain < codomain
            , disabledExplanation = Just "There are no surjective functions when the domain is smaller than the codomain"
            , count = countSurjective
            , generator = genSurjective
            }

        Bijective ->
            { label = "Bijective"
            , isDisabled = \domain codomain -> domain /= codomain
            , disabledExplanation = Just "Bijective functions only exist when the domain and codomain have the same size"
            , count = countBijective
            , generator = \domain _ -> genBijective domain
            }

        Idempotent ->
            { label = "Idempotent"
            , isDisabled = \domain codomain -> domain /= codomain
            , disabledExplanation = Just "Idempotent functions only exist when the domain and codomain have the same size"
            , count = countIdempotent
            , generator = \domain _ -> genIdempotent domain
            }


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

        Generate functionProperty ->
            ( model
            , Random.generate NewRandomMapping <| .generator (propertyConfig functionProperty) model.domain model.codomain
            )

        NewRandomMapping randomList ->
            ( { model | mappings = Array.fromList randomList }, Cmd.none )

        DragStart domainElement ->
            ( { model | dragState = Just { domainElement = domainElement, currentPosition = Nothing } }
            , Cmd.none
            )

        DragOver mouseX mouseY ->
            ( case model.dragState of
                Just dragState ->
                    { model | dragState = Just { dragState | currentPosition = Just ( mouseX, mouseY ) } }

                Nothing ->
                    model
            , Cmd.none
            )

        DragEnd mouseX mouseY ->
            ( case model.dragState of
                Just dragState ->
                    case findTargetCodomainElement mouseX mouseY model.codomain of
                        Just element ->
                            { model
                                | mappings = Array.set (dragState.domainElement - 1) element model.mappings
                                , dragState = Nothing
                            }

                        Nothing ->
                            { model | dragState = Nothing }

                Nothing ->
                    model
            , Cmd.none
            )

        RemoveMapping domainElement ->
            ( { model | mappings = Array.set (domainElement - 1) 0 model.mappings }
            , Cmd.none
            )


isInjective : TotalFunction -> Bool
isInjective (TotalFunction r) =
    let
        mappingList =
            Array.toList r.mappings

        uniqueMappedValues =
            List.Extra.unique mappingList
    in
    List.length mappingList == List.length uniqueMappedValues


isSurjective : TotalFunction -> Bool
isSurjective (TotalFunction f) =
    let
        mappedValues =
            Array.toList f.mappings
                |> List.Extra.unique
                |> List.sort
    in
    List.length mappedValues == f.codomain


isBijective : TotalFunction -> Bool
isBijective ((TotalFunction f) as tf) =
    if f.domain /= f.codomain then
        False

    else
        isInjective tf && isSurjective tf


isIdempotent : TotalFunction -> Bool
isIdempotent ((TotalFunction f) as tf) =
    f.domain == f.codomain && compose tf tf == tf


{-| Count how many sections exist for current function.
A section s of a function f: X → Y is a function s: Y → X such that f ∘ s = idY
-}
countSections : TotalFunction -> Int
countSections ((TotalFunction f) as tf) =
    if isSurjective tf then
        Array.foldl
            (\e ->
                Dict.update e
                    (\mCount ->
                        case mCount of
                            Nothing ->
                                Just 1

                            Just c ->
                                Just (c + 1)
                    )
            )
            Dict.empty
            f.mappings
            |> Dict.values
            |> List.product

    else
        -- Non-surjective functions have no sections
        0


{-| Count how many retractions exist for given function.
A retraction r of a function f: X → Y is a function r: Y → X such that r ∘ f = idX
-}
countRetractions : TotalFunction -> Int
countRetractions ((TotalFunction r) as tf) =
    if isInjective tf then
        let
            unmappedCodomainElems =
                Array.toList r.mappings
                    |> List.Extra.unique
                    |> List.length
                    |> (\mappedCount -> r.codomain - mappedCount)
        in
        r.domain ^ unmappedCodomainElems

    else
        -- Non-injective functions have no retractions
        0


view : Model -> Html Msg
view ({ domain, codomain } as model) =
    Html.div [ A.class "app-container" ]
        [ Html.div [ A.class "header" ]
            [ Html.h1 [ A.class "title" ] [ Html.text "Function Visualizer" ] ]
        , Html.div [ A.class "main-content" ]
            [ Html.div [ A.class "controls-panel" ]
                [ Html.div [ A.class "control-group" ]
                    [ Html.h2 [ A.class "section-header" ] [ Html.text "Set Configuration" ]
                    , setSizeInput "Domain size:" domain UpdateDomain
                    , setSizeInput "Codomain size:" codomain UpdateCodomain
                    ]
                , Html.div [ A.class "control-group" ]
                    [ Html.h2 [ A.class "section-header" ] [ Html.text "Generate Function" ]
                    , Html.div [ A.class "function-buttons" ]
                        (List.map (\property -> viewFunctionButton property (propertyConfig property) domain codomain)
                            [ Any, Injective, Surjective, Bijective, Idempotent ]
                        )
                    ]
                , Html.div [ A.class "control-group" ]
                    [ Html.h2 [ A.class "section-header" ] [ Html.text "Function Mappings" ]
                    , Html.div []
                        (List.indexedMap (viewMapping model) (Array.toList model.mappings))
                    ]
                ]
            , Html.div [ A.class "visualization-panel" ]
                [ viewSvgMapping model
                , viewFunctionStats model
                ]
            ]
        , Html.node "style" [] [ Html.text styles ]
        ]


viewFunctionStats : Model -> Html msg
viewFunctionStats model =
    let
        mTotal =
            mkTotal model

        viewStat getStat =
            case mTotal of
                Nothing ->
                    "N/A"

                Just totalFunction ->
                    getStat totalFunction

        boolToStr : Bool -> String
        boolToStr b =
            if b then
                "Yes"

            else
                "No"
    in
    Html.div [ A.class "function-stats" ]
        [ Html.h2 [ A.class "section-header" ] [ Html.text "Function Stats" ]
        , Html.div [ A.class "stats-container" ]
            [ Html.div [ A.class "stats-row" ]
                [ Html.span [ A.class "stats-label" ] [ Html.text "Properties:" ]
                ]
            , Html.div [ A.class "stats-row" ]
                [ Html.span [ A.class "stats-property" ] [ Html.text "Injective:" ]
                , Html.span [] [ Html.text (viewStat (isInjective >> boolToStr)) ]
                ]
            , Html.div [ A.class "stats-row" ]
                [ Html.span [ A.class "stats-property" ] [ Html.text "Surjective:" ]
                , Html.span [] [ Html.text (viewStat (isSurjective >> boolToStr)) ]
                ]
            , Html.div [ A.class "stats-row" ]
                [ Html.span [ A.class "stats-property" ] [ Html.text "Bijective:" ]
                , Html.span [] [ Html.text (viewStat (isBijective >> boolToStr)) ]
                ]
            , Html.div [ A.class "stats-row" ]
                [ Html.span [ A.class "stats-property" ] [ Html.text "Idempotent:" ]
                , Html.span [] [ Html.text (viewStat (isIdempotent >> boolToStr)) ]
                ]
            , Html.div [ A.class "stats-row" ]
                [ Html.span [ A.class "stats-property" ] [ Html.text "Number of sections:" ]
                , Html.span [] [ Html.text (viewStat (countSections >> String.fromInt)) ]
                ]
            , Html.div [ A.class "stats-row" ]
                [ Html.span [ A.class "stats-property" ] [ Html.text "Number of retractions:" ]
                , Html.span [] [ Html.text (viewStat (countRetractions >> String.fromInt)) ]
                ]
            ]
        ]


viewFunctionButton : FunctionProperty -> PropertyConfig -> Int -> Int -> Html Msg
viewFunctionButton functionProperty ({ label, isDisabled } as cfg) domain codomain =
    let
        disabled =
            isDisabled domain codomain

        count =
            cfg.count domain codomain
    in
    Html.div []
        [ Html.button
            [ E.onClick (Generate functionProperty)
            , A.disabled disabled
            , A.title <|
                if disabled then
                    Maybe.withDefault "" cfg.disabledExplanation

                else
                    "Generate random " ++ String.toLower label ++ " function"
            , A.class "function-button"
            ]
            [ Html.text label ]
        , Html.span
            []
            [ Html.text (" (" ++ String.fromInt count ++ ")") ]
        ]


setSizeInput : String -> Int -> (String -> Msg) -> Html Msg
setSizeInput label value toMsg =
    Html.div [ A.class "input-container" ]
        [ Html.label [ A.class "input-label" ] [ Html.text label ]
        , Html.input
            [ A.type_ "number"
            , A.min "0"
            , A.max (String.fromInt maxSetSize)
            , A.value (String.fromInt value)
            , E.onInput toMsg
            , A.class "number-input"
            ]
            []
        ]


viewMapping : Model -> Int -> Int -> Html Msg
viewMapping model index value =
    Html.div [ A.class "mapping-row" ]
        [ Html.span [ A.class "mapping-label" ]
            [ Html.text ("f(" ++ String.fromInt (index + 1) ++ ") = ") ]
        , Html.select
            [ E.onInput (UpdateMapping index)
            , A.class "mapping-select"
            ]
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
            (1 + max model.domain model.codomain) * gridUnit

        domainCircles =
            List.range 1 model.domain
                |> List.map (\i -> viewDomainCircle model i (Array.get (i - 1) model.mappings == Just 0))

        codomainCircles =
            List.range 1 model.codomain
                |> List.map (viewCodomainCircle model)

        arrows =
            Array.toIndexedList model.mappings
                |> List.filter
                    (\( i, v ) ->
                        -- Don't show arrows for elements whose mapping is not specified
                        (v /= 0)
                            && -- Don't show arrow for dragged domain element
                               (Just (i + 1) /= Maybe.map .domainElement model.dragState)
                    )
                |> List.map (\( i, v ) -> viewArrow (i + 1) v)

        dragLine =
            case model.dragState of
                Just { domainElement, currentPosition } ->
                    case currentPosition of
                        Just ( x2, y2 ) ->
                            let
                                x1 =
                                    gridUnit + circleRadius

                                y1 =
                                    domainElement * gridUnit
                            in
                            [ Svg.line
                                [ SA.x1 (String.fromInt x1)
                                , SA.y1 (String.fromInt y1)
                                , SA.x2 (String.fromInt x2)
                                , SA.y2 (String.fromInt y2)
                                , SA.stroke "blue"
                                , SA.strokeWidth "1.5"
                                , SA.strokeDasharray "5,5"

                                -- TODO add arrowhead, but why is it bigger than the arrowhead for static arrows?
                                --, SA.markerEnd "url(#arrowhead)"
                                ]
                                []
                            ]

                        Nothing ->
                            []

                Nothing ->
                    []
    in
    Html.div [ A.class "svg-container" ]
        [ Html.h2 [ A.class "section-header" ] [ Html.text "Function Visualization" ]
        , Html.div [ A.class "instructions" ]
            [ Html.text "Drag from domain to codomain to create mapping. Click on domain element to clear mapping." ]
        , Svg.svg
            [ SA.width (String.fromInt svgWidth)
            , SA.height (String.fromInt svgHeight)
            , SA.viewBox ("0 0 " ++ String.fromInt svgWidth ++ " " ++ String.fromInt svgHeight)
            , onSvgMouseMove DragOver
            , onSvgMouseUp DragEnd
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
                    [ Svg.polygon [ SA.points "0 0, 10 3.5, 0 7", SA.fill "black" ] [] ]
                ]
                :: domainCircles
                ++ codomainCircles
                ++ arrows
                ++ dragLine
            )
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


viewDomainCircle : Model -> Int -> Bool -> Svg Msg
viewDomainCircle model domainElement isUnmapped =
    let
        x =
            gridUnit

        y =
            domainElement * gridUnit

        isDragging =
            model.dragState
                |> Maybe.map (\ds -> ds.domainElement == domainElement)
                |> Maybe.withDefault False

        fill =
            if isDragging then
                -- Light blue when dragging
                "#e6f7ff"

            else if isUnmapped then
                -- Light red when unmapped
                "#ffe6e6"

            else
                -- White for mapped elements
                "#fff"
    in
    Svg.g
        [ onSvgMouseDown (DragStart domainElement)
        , SE.onClick (RemoveMapping domainElement)
        ]
        [ Svg.circle
            [ SA.cx (String.fromInt x)
            , SA.cy (String.fromInt y)
            , SA.r (String.fromInt circleRadius)
            , SA.fill fill
            , SA.strokeWidth "1"
            , SA.stroke <|
                if isUnmapped then
                    "red"

                else if isDragging then
                    "blue"

                else
                    "black"
            , SA.cursor "grab"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromInt x)
            , SA.y (String.fromInt y)
            , SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.fontFamily "sans-serif"
            , SA.fontSize "12px"
            , SA.fill "black"
            ]
            [ Svg.text (String.fromInt domainElement) ]
        ]


viewCodomainCircle : Model -> Int -> Svg Msg
viewCodomainCircle model codomainElement =
    let
        x =
            domainCodomainGridDist * gridUnit

        y =
            codomainElement * gridUnit

        isDragTarget =
            model.dragState /= Nothing
    in
    Svg.g []
        [ Svg.circle
            [ SA.cx (String.fromInt x)
            , SA.cy (String.fromInt y)
            , SA.r (String.fromInt circleRadius)
            , SA.fill "transparent"
            , SA.strokeWidth "1"
            , SA.stroke "black"
            , SA.cursor
                (if isDragTarget then
                    "pointer"

                 else
                    "default"
                )
            ]
            []
        , Svg.text_
            [ SA.x (String.fromInt x)
            , SA.y (String.fromInt y)
            , SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.fontFamily "sans-serif"
            , SA.fontSize "12px"
            , SA.fill "black"
            ]
            [ Svg.text (String.fromInt codomainElement) ]
        ]


onSvgMouseDown : Msg -> Svg.Attribute Msg
onSvgMouseDown msg =
    SE.on "mousedown" (Decode.succeed msg)


onSvgMouseUp : (Int -> Int -> Msg) -> Svg.Attribute Msg
onSvgMouseUp toMsg =
    SE.on "mouseup" (decodeMousePosition toMsg)


onSvgMouseMove : (Int -> Int -> Msg) -> Svg.Attribute Msg
onSvgMouseMove toMsg =
    SE.on "mousemove" (decodeMousePosition toMsg)


decodeMousePosition : (Int -> Int -> msg) -> Decoder msg
decodeMousePosition toMsg =
    Decode.map2 toMsg
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


findTargetCodomainElement : Int -> Int -> Int -> Maybe Int
findTargetCodomainElement mouseX mouseY codomainSize =
    let
        -- The X coordinate must be near the codomain column to be considered a valid target
        codomainX =
            domainCodomainGridDist * gridUnit

        isNearCodomainColumn =
            abs (mouseX - codomainX) <= (circleRadius + 5)

        -- If in the correct X range, calculate which element we're near based on Y coordinate
        potentialElement =
            round (toFloat mouseY / toFloat gridUnit)

        -- Only consider it a match if we're close enough to a codomain element
        isValidCodomainElement =
            1 <= potentialElement && potentialElement <= codomainSize
    in
    if isNearCodomainColumn && isValidCodomainElement then
        Just potentialElement

    else
        Nothing


maxSetSize : Int
maxSetSize =
    10


domainCodomainGridDist : Int
domainCodomainGridDist =
    5


circleRadius : Int
circleRadius =
    16


gridUnit : Int
gridUnit =
    45


{-| Calculate number of all possible functions from domain to codomain
-}
countAllFunctions : Int -> Int -> Int
countAllFunctions domain codomain =
    codomain ^ domain


{-| Calculate number of injective functions from domain to codomain
-}
countInjective : Int -> Int -> Int
countInjective domain codomain =
    if domain > codomain then
        0

    else
        List.range 0 (domain - 1)
            |> List.foldl (\i acc -> acc * (codomain - i)) 1


{-| Calculate number of surjective functions from domain to codomain
-}
countSurjective : Int -> Int -> Int
countSurjective domain codomain =
    if domain < codomain then
        0

    else if domain == 0 && codomain == 0 then
        1

    else if codomain == 0 then
        0

    else
        List.range 1 codomain
            |> List.foldl
                (\k acc ->
                    acc + (-1 ^ (codomain - k)) * binomial codomain k * (k ^ domain)
                )
                0


{-| Calculate number of bijective functions (permutations) for equal-sized sets
-}
countBijective : Int -> Int -> Int
countBijective domain codomain =
    if domain /= codomain then
        0

    else
        factorial domain


{-| Calculate factorial of n (n!)
-}
factorial : Int -> Int
factorial n =
    if n <= 1 then
        1

    else
        List.range 1 n |> List.product


{-| Calculate binomial coefficient (n choose k)
Uses an iterative approach to avoid large factorial calculations
and potential overflows, also applies the symmetry property: C(n,k) = C(n,n-k)
-}
binomial : Int -> Int -> Int
binomial n k =
    if k < 0 || k > n then
        0

    else if k == 0 || k == n then
        1

    else
        -- Use symmetry to reduce number of multiplications needed
        let
            k_ =
                min k (n - k)
        in
        List.range 1 k_
            |> List.foldl
                (\i ( numerator, denominator ) ->
                    ( numerator * (n - i + 1)
                    , denominator * i
                    )
                )
                ( 1, 1 )
            |> (\( numerator, denominator ) -> numerator // denominator)


genAny : Int -> Int -> Random.Generator (List Int)
genAny domain codomain =
    Random.list domain (Random.int 1 codomain)


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


{-| Generate a random idempotent function
An idempotent function f satisfies f(f(x)) = f(x) for all x in the domain
This implementation uses a weighted approach to select the number of fixed points
based on the distribution of idempotent functions with different numbers of fixed points.
-}
genIdempotent : Int -> Random.Generator (List Int)
genIdempotent size =
    let
        -- Step 1: For each possible number of fixed points, calculate the number
        -- of idempotent functions with that many fixed points.
        weightedFixedPointCounts =
            List.range 1 size
                |> List.filterMap
                    (\k ->
                        let
                            weight =
                                toFloat (binomial size k * k ^ (size - k))
                        in
                        if weight > 0 then
                            Just ( weight, k )

                        else
                            Nothing
                    )
    in
    -- Step 2: Select the number of fixed points based on the calculated weights
    case weightedFixedPointCounts of
        [] ->
            Random.constant []

        w :: ws ->
            Random.weighted w ws
                |> Random.andThen
                    (\numFixedPoints ->
                        -- Step 3: Select which domain elements will be fixed points
                        Random.List.shuffle (List.range 1 size)
                            |> Random.map (List.Extra.splitAt numFixedPoints)
                            |> Random.andThen
                                (\( fixedPoints, nonFixedPoints ) ->
                                    let
                                        fixedPointMappings =
                                            List.map (\x -> ( x, x )) fixedPoints

                                        -- Step 4: For each non-fixed point, select a random fixed point to map to
                                        generateNonFixedMappings remainingPoints mappingsAcc =
                                            case remainingPoints of
                                                [] ->
                                                    Random.constant mappingsAcc

                                                x :: xs ->
                                                    -- Select a random fixed point to map to
                                                    Random.uniform
                                                        (List.head fixedPoints |> Maybe.withDefault 1)
                                                        (List.tail fixedPoints |> Maybe.withDefault [])
                                                        |> Random.andThen
                                                            (\selectedFixedPoint ->
                                                                generateNonFixedMappings xs (( x, selectedFixedPoint ) :: mappingsAcc)
                                                            )
                                    in
                                    generateNonFixedMappings nonFixedPoints fixedPointMappings
                                        |> Random.map
                                            (\mappings ->
                                                mappings
                                                    |> List.sortBy Tuple.first
                                                    |> List.map Tuple.second
                                            )
                                )
                    )


{-| Count the number of idempotent functions for a given domain size
For a set of size n, the number of idempotent functions is the sum of
(n choose k) \* k^(n-k) for k from 1 to n, where k is the number of fixed points
-}
countIdempotent : Int -> Int -> Int
countIdempotent domain codomain =
    if domain /= codomain then
        0

    else if domain == 0 then
        1

    else
        List.range 1 domain
            |> List.map (\k -> binomial domain k * k ^ (domain - k))
            |> List.sum


styles : String
styles =
    """
* {
    box-sizing: border-box;
}

.app-container {
    font-family: sans-serif;
    max-width: 1000px;
    margin: 0 auto;
    padding: 10px;
}

.header {
    margin-bottom: 15px;
    padding-bottom: 10px;
    border-bottom: 1px solid #ccc;
}

.main-content {
    display: flex;
    flex-wrap: wrap;
    gap: 15px;
}

.controls-panel {
    flex: 1 1 300px;
}

.visualization-panel {
    flex: 1 1 400px;
}

.control-group {
    margin-bottom: 15px;
    padding: 10px;
    border: 1px solid #ccc;
}

.section-header {
    margin: 5px 0;
}

.input-container {
    display: flex;
    align-items: center;
    margin-bottom: 8px;
}

.input-label {
    flex: 0 0 120px;
}

.function-buttons {
    display: flex;
    flex-direction: column;
    gap: 6px;
}

.function-button {
    width: 90px;
    padding: 2px 5px;
}

.mapping-row {
    display: flex;
    margin-bottom: 4px;
    align-items: center;
}

.mapping-label {
    flex: 0 0 55px;
    text-align: right;
    padding-right: 5px;
}

.svg-container {
    padding: 10px;
    border: 1px solid #ccc;
    user-select: none;
    touch-action: none;
}

.instructions {
    font-size: 12px;
    margin-bottom: 8px;
    color: #555;
    font-style: italic;
}

.function-stats {
    margin-top: 15px;
    padding: 10px;
    border: 1px solid #ccc;
}

.stats-container {
    margin-left: 10px;
}

.stats-row {
    margin-bottom: 5px;
}

.stats-label {
    font-weight: bold;
}

.stats-property {
    margin-left: 10px;
    display: inline-block;
    width: 170px;
}
    """
