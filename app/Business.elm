module Business exposing (..)

import Html exposing (Html, text)
import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Typography as Typography
import Material.Grid exposing (grid, cell, size, Device(..), Align(..), align)
import Models exposing (..)
import Utils exposing (demandPercentage)
import FormatNumber exposing (formatFloat, formatInt, usLocale)


init : BusinessModule
init =
    { funds = 0
    , inventory = 0
    , price = 0.25
    , demand = 3
    , demandBoost = 1
    , marketingCost = 100
    , marketingLvl = 1
    , marketingEffectiveness = 1
    }


view : Model -> Html Msg
view model =
    let
        businessModule =
            model.businessModule
    in
        Card.view
            [ Elevation.e2
            , css "margin" "4px 8px"
            , css "width" "400px"
            ]
            [ Card.title
                [ css "flex-direction" "column" ]
                [ Card.head [] [ text "Business" ]
                , Card.subhead [] [ text ("Available Funds: $ " ++ (formatFloat usLocale businessModule.funds)) ]
                ]
            , Card.text
                [ Color.text Color.black
                , Typography.subhead
                ]
                [ text ("Unsold Inventory: " ++ (formatInt usLocale businessModule.inventory)) ]
            , Card.actions [ Color.text Color.black ]
                [ grid []
                    [ cell
                        [ size All 12
                        , align Middle
                        ]
                        [ Button.render Mdl
                            [ 1, 1 ]
                            model.mdl
                            [ Button.colored
                            , Button.ripple
                            , Options.onClick LowerPrice
                            ]
                            [ text "-"
                            ]
                        , Button.render Mdl
                            [ 1, 2 ]
                            model.mdl
                            [ Button.colored
                            , Button.ripple
                            , Options.onClick RaisePrice
                            ]
                            [ text "+"
                            ]
                        , text
                            (" Price per Pastel: $ "
                                ++ (formatFloat usLocale businessModule.price)
                            )
                        ]
                    , cell
                        [ size All 12
                        , align Top
                        ]
                        [ Options.span [] [ text ("Public demand: " ++ (demandPercentage businessModule.demand) ++ "%") ]
                        ]
                    ]
                ]
            , Card.actions [ Color.text Color.black ]
                [ grid []
                    [ cell [ size All 6 ]
                        [ Button.render Mdl
                            [ 1, 3 ]
                            model.mdl
                            [ Button.colored
                            , Button.ripple
                            , Options.onClick BuyAds
                            , Options.disabled (businessModule.funds < (toFloat businessModule.marketingCost))
                            ]
                            [ text "Marketing"
                            ]
                        ]
                    , cell
                        [ size All 6
                        , align Middle
                        ]
                        [ text (" Level: " ++ (toString businessModule.marketingLvl))
                        ]
                    , cell
                        [ size All 12
                        , align Top
                        ]
                        [ Options.span [] [ text ("Cost: $ " ++ (formatInt usLocale businessModule.marketingCost)) ]
                        ]
                    ]
                ]
            ]


updateModel : BusinessModule -> BusinessModule
updateModel model =
    let
        marketing =
            (1.1 ^ toFloat (model.marketingLvl - 1))

        demand =
            (((0.8 / model.price) * marketing * toFloat model.marketingEffectiveness) * (toFloat model.demandBoost) * 1.1)
    in
        { model
            | demand = demand
        }


lowerPrice : BusinessModule -> BusinessModule
lowerPrice model =
    { model
        | price = (Basics.max (model.price - 0.01) 0.01)
    }


raisePrice : BusinessModule -> BusinessModule
raisePrice model =
    { model
        | price = model.price + 0.01
    }


sellPasteis : BusinessModule -> Float -> BusinessModule
sellPasteis model rand =
    let
        demand =
            floor (0.7 * (model.demand ^ 1.15))
    in
        if (rand > model.demand || model.inventory == 0) then
            model
        else if (demand > model.inventory) then
            { model
                | inventory = 0
                , funds = model.funds + (model.price * (toFloat model.inventory))
            }
        else
            { model
                | inventory = model.inventory - demand
                , funds = model.funds + (model.price * (toFloat demand))
            }


addFunds : BusinessModule -> Float -> BusinessModule
addFunds model income =
    { model
        | funds = model.funds + income
    }


removeFunds : BusinessModule -> Float -> BusinessModule
removeFunds model outcome =
    { model
        | funds = model.funds - outcome
    }


addItems : BusinessModule -> Int -> BusinessModule
addItems model income =
    { model
        | inventory = model.inventory + income
    }


removeItems : BusinessModule -> Int -> BusinessModule
removeItems model outcome =
    { model
        | inventory = model.inventory - outcome
    }


buyAds : BusinessModule -> BusinessModule
buyAds model =
    if (model.funds < toFloat model.marketingCost) then
        model
    else
        { model
            | funds = model.funds - toFloat model.marketingCost
            , marketingCost = model.marketingCost * 2
            , marketingLvl = model.marketingLvl + 1
        }
