module Manufacturing exposing (..)

import Html exposing (Html, text)
import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Grid exposing (grid, cell, size, Device(..), Align(..), align)
import Models exposing (..)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Business as Business


init : ManufacturingModule
init =
    { dough = 1000
    , doughSupply = 1000
    , doughCost = 15
    , doughBasePrice = 15
    , pasteisModule = Nothing
    , megaPasteisModule = Nothing
    , partialPasteis = 0
    , pasteisMakerRate = 0
    }


view : Model -> Html Msg
view model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        Card.view
            [ Elevation.e2
            , css "margin" "4px 8px"
            , css "width" "400px"
            ]
            [ Card.title
                [ css "flex-direction" "column" ]
                [ Card.head [] [ text "Manufacturing" ]
                , Card.subhead [] [ text ("Pasteis per Second: " ++ (manufacturingModule.pasteisMakerRate |> round |> toString)) ]
                ]
            , Card.actions
                [ Color.text Color.black ]
                [ grid []
                    [ cell [ size All 6 ]
                        [ Button.render Mdl
                            [ 2, 1 ]
                            model.mdl
                            [ Button.colored
                            , Button.ripple
                            , Options.onClick BuyDough
                            , Options.disabled (businessModule.funds < (toFloat manufacturingModule.doughCost))
                            ]
                            [ text "Dough"
                            ]
                        ]
                    , cell
                        [ size All 6
                        , align Middle
                        ]
                        [ text (" " ++ (toString manufacturingModule.dough) ++ " coins")
                        ]
                    , cell
                        [ size All 12
                        , align Top
                        ]
                        [ text ("Cost: $ " ++ (toString manufacturingModule.doughCost))
                        ]
                    ]
                ]
            , pasteisView model
            , megaPasteisView model
            ]


pasteisView : Model -> Card.Block Msg
pasteisView model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        case manufacturingModule.pasteisModule of
            Nothing ->
                Card.actions [ Color.text Color.black ] [ text "" ]

            Just mod ->
                Card.actions
                    [ Color.text Color.black
                    , Card.border
                    ]
                    [ grid []
                        [ cell [ size All 6 ]
                            [ Button.render Mdl
                                [ 2, 2 ]
                                model.mdl
                                [ Button.colored
                                , Button.ripple
                                , Options.onClick BuyPasteis
                                , Options.disabled (businessModule.funds < mod.cost)
                                ]
                                [ text "AutoPasteis"
                                ]
                            ]
                        , cell
                            [ size All 6
                            , align Middle
                            ]
                            [ text (" " ++ (toString mod.level))
                            ]
                        , cell
                            [ size All 12
                            , align Top
                            ]
                            [ Options.span [] [ text ("Cost: $ " ++ (formatFloat usLocale mod.cost)) ]
                            ]
                        ]
                    ]


megaPasteisView : Model -> Card.Block Msg
megaPasteisView model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        case manufacturingModule.megaPasteisModule of
            Nothing ->
                Card.actions [ Color.text Color.black ] [ text "" ]

            Just mod ->
                Card.actions
                    [ Color.text Color.black
                    , Card.border
                    ]
                    [ grid []
                        [ cell [ size All 6 ]
                            [ Button.render Mdl
                                [ 2, 3 ]
                                model.mdl
                                [ Button.colored
                                , Button.ripple
                                , Options.onClick BuyMegaPasteis
                                , Options.disabled (businessModule.funds < mod.cost)
                                ]
                                [ text "MegaPasteis"
                                ]
                            ]
                        , cell
                            [ size All 6
                            , align Middle
                            ]
                            [ text (" " ++ (toString mod.level))
                            ]
                        , cell
                            [ size All 12
                            , align Top
                            ]
                            [ Options.span [] [ text ("Cost: $ " ++ (formatFloat usLocale mod.cost)) ]
                            ]
                        ]
                    ]


updateModel : ManufacturingModule -> BusinessModule -> ManufacturingModule
updateModel model business =
    { model
        | pasteisModule = (tryMakePasteisModule model business.funds)
        , megaPasteisModule = (tryMakeMegaPasteisModule model)
    }


tryMakePasteisModule : ManufacturingModule -> Float -> Maybe PasteisModule
tryMakePasteisModule model funds =
    case model.pasteisModule of
        Just mod ->
            let
                pasteisCost =
                    (1.1 ^ (toFloat mod.level)) + 4
            in
                Just { mod | cost = pasteisCost }

        Nothing ->
            let
                enoughFunds =
                    funds >= 5
            in
                case enoughFunds of
                    False ->
                        Nothing

                    True ->
                        Just
                            { cost = 5
                            , boost = 1
                            , level = 0
                            }


tryMakeMegaPasteisModule : ManufacturingModule -> Maybe MegaPasteisModule
tryMakeMegaPasteisModule model =
    case model.megaPasteisModule of
        Just mod ->
            let
                megaPasteisCost =
                    (1.07 ^ (toFloat mod.level)) * 1000
            in
                Just { mod | cost = megaPasteisCost }

        Nothing ->
            case model.pasteisModule of
                Nothing ->
                    Nothing

                Just pasteisModule ->
                    case pasteisModule.level >= 75 of
                        False ->
                            Nothing

                        True ->
                            Just
                                { cost = 1000
                                , boost = 1
                                , level = 0
                                }


addPasteis : ManufacturingModule -> ManufacturingModule
addPasteis model =
    case model.pasteisModule of
        Nothing ->
            model

        Just mod ->
            { model | pasteisModule = addPasteis_ mod }


addPasteis_ : PasteisModule -> Maybe PasteisModule
addPasteis_ model =
    Just { model | level = model.level + 1 }


addMegaPasteis : ManufacturingModule -> ManufacturingModule
addMegaPasteis model =
    case model.megaPasteisModule of
        Nothing ->
            model

        Just mod ->
            { model | megaPasteisModule = addMegaPasteis_ mod }


addMegaPasteis_ : MegaPasteisModule -> Maybe MegaPasteisModule
addMegaPasteis_ model =
    Just { model | level = model.level + 1 }


adjustdoughCost : ManufacturingModule -> Float -> ManufacturingModule
adjustdoughCost model rand =
    if (rand > 1.5) then
        model
    else
        let
            doughAdjust =
                5 * (Basics.sin ((rand * 100) + 50))
        in
            { model
                | doughCost = ceiling (model.doughBasePrice + doughAdjust)
            }


createPastel : Model -> Model
createPastel model =
    if (model.manufacturingModule.dough < 1) then
        model
    else
        let
            businessModule =
                Business.addItems model.businessModule 1

            manufacturingModule =
                model.manufacturingModule

            newManufacturingModule =
                { manufacturingModule | dough = manufacturingModule.dough - 1 }
        in
            { model
                | pasteis = model.pasteis + 1
                , businessModule = businessModule
                , manufacturingModule = newManufacturingModule
            }


makePasteis : Model -> Model
makePasteis model =
    case model.manufacturingModule.dough of
        0 ->
            let
                manufacturingModule =
                    model.manufacturingModule

                newManufacturingModule =
                    { manufacturingModule
                        | pasteisMakerRate = 0
                        , partialPasteis = 0
                    }
            in
                { model
                    | manufacturingModule = newManufacturingModule
                }

        _ ->
            let
                autoPasteisAmount =
                    runPasteis model.manufacturingModule.pasteisModule

                megaPasteisAmount =
                    runMegaPasteis model.manufacturingModule.megaPasteisModule

                partialPasteisCapacity =
                    model.manufacturingModule.partialPasteis + autoPasteisAmount + megaPasteisAmount

                fullPasteisCapacity =
                    floor partialPasteisCapacity

                pasteisMakerRate =
                    (Basics.min (autoPasteisAmount + megaPasteisAmount) (toFloat model.manufacturingModule.dough)) * 10

                fullPasteis =
                    Basics.min fullPasteisCapacity model.manufacturingModule.dough

                manufacturingModule =
                    model.manufacturingModule

                newManufacturingModule =
                    { manufacturingModule
                        | partialPasteis = partialPasteisCapacity - (toFloat fullPasteisCapacity)
                        , dough = model.manufacturingModule.dough - fullPasteis
                        , pasteisMakerRate = pasteisMakerRate
                    }
            in
                { model
                    | pasteis = model.pasteis + fullPasteis
                    , businessModule = Business.addItems model.businessModule fullPasteis
                    , manufacturingModule = newManufacturingModule
                }


runPasteis : Maybe PasteisModule -> Float
runPasteis model =
    case model of
        Nothing ->
            0

        Just mod ->
            (toFloat (mod.boost * mod.level)) / 10


runMegaPasteis : Maybe MegaPasteisModule -> Float
runMegaPasteis model =
    case model of
        Nothing ->
            0

        Just mod ->
            (toFloat (mod.boost * mod.level)) / 10


buyDough : ManufacturingModule -> BusinessModule -> ( ManufacturingModule, BusinessModule )
buyDough model business =
    let
        doughCost =
            toFloat model.doughCost
    in
        if (business.funds < doughCost) then
            ( model, business )
        else
            let
                businessModule =
                    Business.removeFunds business doughCost

                manufacturingModule =
                    { model
                        | doughBasePrice = model.doughBasePrice + 0.05
                        , dough = model.dough + model.doughSupply
                    }
            in
                ( manufacturingModule, businessModule )
