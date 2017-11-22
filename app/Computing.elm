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
    { trust = 0
    , processors = 1
    , memory = 1
    , memoryLimit = 1000
    , operations = 1000
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
                        [ cell [ size All 6 ]
                            [ Button.render Mdl
                                [ 3, 1 ]
                                model.mdl
                                [ Button.colored
                                , Button.ripple
                                , Options.onClick AddProcessor
                                , Options.disabled (mod.trust < 1)
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
                                , Options.disabled (mod.trust < 1)
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
                                    ++ (formatInt usLocale mod.operations)
                                    ++ " / "
                                    ++ (formatInt usLocale mod.memoryLimit)
                                )
                            ]
                        , cell
                            [ size All 12
                            , align Middle
                            ]
                            [ text ("Creativity : " ++ (formatInt usLocale mod.creativity))
                            ]
                        ]
                    ]
                ]


updateModel : ComputingModule -> ComputingModule
updateModel model =
    { model
        | memoryLimit = model.memory * 1000
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
    case model.trust of
        0 ->
            model

        _ ->
            { model
                | processors = model.processors + 1
                , trust = model.trust - 1
            }


addMemory : ComputingModule -> ComputingModule
addMemory model =
    case model.trust of
        0 ->
            model

        _ ->
            { model
                | memory = model.memory + 1
                , trust = model.trust - 1
            }
