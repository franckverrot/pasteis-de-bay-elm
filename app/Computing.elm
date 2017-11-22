module Computing exposing (..)

import Html exposing (Html, text)
import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Typography as Typography
import Material.Grid exposing (grid, cell, size, Device(..), Align(..), align)
import Models exposing (..)
import FormatNumber exposing (formatFloat, formatInt, usLocale)


init : ComputingModule
init =
    { trust = 2
    , processors = 1
    , memory = 1
    , memoryLimit = 1000
    , operations = 0
    , creativityEnable = False
    , creativity = 0
    }


view : Model -> Html Msg
view model =
    case model.computingModule of
        Nothing ->
            text ""

        Just mod ->
            Card.view
                [ Elevation.e2
                , css "margin" "4px 8px"
                , css "width" "100%"
                ]
                [ Card.title
                    [ css "flex-direction" "column" ]
                    [ Card.head [] [ text "Research" ]
                    , Card.subhead [] [ text ("Trust : " ++ (formatInt usLocale mod.trust)) ]
                    ]
                , Card.actions
                    [ Color.text Color.black ]
                    [ grid []
                        [ cell
                            [ size All 12
                            , align Middle
                            ]
                            [ text ("Next trust " ++ (formatInt usLocale ((nextTrust mod.trust) * 1000)))
                            ]
                        , cell [ size All 6 ]
                            [ Button.render Mdl
                                [ 3, 1 ]
                                model.mdl
                                [ Button.colored
                                , Button.ripple
                                , Options.onClick AddProcessor
                                , Options.disabled ((mod.trust - (mod.processors + mod.memory)) < 1)
                                ]
                                [ text "Processors"
                                ]
                            ]
                        , cell
                            [ size All 6
                            , align Middle
                            ]
                            [ text (" " ++ (formatInt usLocale mod.processors))
                            ]
                        , cell [ size All 6 ]
                            [ Button.render Mdl
                                [ 3, 2 ]
                                model.mdl
                                [ Button.colored
                                , Button.ripple
                                , Options.onClick AddMemory
                                , Options.disabled ((mod.trust - (mod.processors + mod.memory)) < 1)
                                ]
                                [ text "Memory"
                                ]
                            ]
                        , cell
                            [ size All 6
                            , align Middle
                            ]
                            [ text (" " ++ (formatInt usLocale mod.memory))
                            ]
                        ]
                    ]
                , Card.actions
                    [ Color.text Color.black ]
                    [ grid []
                        [ cell
                            [ size All 12
                            , align Middle
                            ]
                            [ text
                                ("Operations : "
                                    ++ (formatInt usLocale (floor mod.operations))
                                    ++ " / "
                                    ++ (formatInt usLocale mod.memoryLimit)
                                )
                            ]
                        , cell
                            [ size All 12
                            , align Middle
                            ]
                            [ text ("Creativity : " ++ (formatInt usLocale (floor mod.creativity)))
                            ]
                        ]
                    ]
                ]


updateModel : Model -> Maybe ComputingModule
updateModel model =
    case model.computingModule of
        Nothing ->
            Nothing

        Just mod ->
            let
                addTrust =
                    model.pasteis > ((nextTrust mod.trust) * 1000)

                log =
                    Debug.log "pasteis" model.pasteis

                log2 =
                    Debug.log "nextTrust" ((nextTrust mod.trust) * 1000)

                log3 =
                    Debug.log "addTrust" addTrust
            in
                case addTrust of
                    False ->
                        Just
                            { mod
                                | memoryLimit = mod.memory * 1000
                            }

                    True ->
                        Just
                            { mod
                                | memoryLimit = mod.memory * 1000
                                , trust = mod.trust + 1
                            }


tryMakeComputingModule : Model -> Model
tryMakeComputingModule model =
    case model.computingModule of
        Just mod ->
            model

        Nothing ->
            let
                enoughPasteis =
                    model.pasteis >= 2000
            in
                case enoughPasteis of
                    False ->
                        model

                    True ->
                        { model | computingModule = Just init }


addProcessor : ComputingModule -> ComputingModule
addProcessor model =
    let
        availableTrust =
            (model.trust - (model.processors + model.memory)) < 1
    in
        case availableTrust of
            False ->
                model

            True ->
                { model
                    | processors = model.processors + 1
                }


addMemory : ComputingModule -> ComputingModule
addMemory model =
    let
        availableTrust =
            (model.trust - (model.processors + model.memory)) < 1
    in
        case availableTrust of
            False ->
                model

            True ->
                { model
                    | memory = model.memory + 1
                }


makeOperations : ComputingModule -> ComputingModule
makeOperations model =
    let
        full =
            model.operations >= (toFloat model.memoryLimit)
    in
        case full of
            True ->
                model |> makeCreativity

            False ->
                let
                    newOps =
                        (toFloat model.processors) / 10

                    operations =
                        Basics.min (model.operations + newOps) (toFloat model.memoryLimit)
                in
                    { model | operations = operations }


makeCreativity : ComputingModule -> ComputingModule
makeCreativity model =
    case model.creativityEnable of
        False ->
            model

        True ->
            let
                creativitySpeed =
                    ((logBase 10 (toFloat model.processors)) * ((toFloat model.processors) ^ 1.1)) + toFloat (model.processors - 1)

                newCreativity =
                    creativitySpeed / 400

                creativity =
                    model.creativity + newCreativity
            in
                { model | creativity = creativity }


nextTrust : Int -> Int
nextTrust x =
    case x of
        0 ->
            1

        1 ->
            1

        _ ->
            nextTrust (x - 1) + nextTrust (x - 2)
