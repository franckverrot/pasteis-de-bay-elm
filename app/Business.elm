module Business exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


view : BusinessModule -> Html Msg
view model =
    div []
        [ div []
            [ h2 [] [ text "Business" ]
            ]
        , div []
            [ span []
                [ text ("Available Funds: $ " ++ (formatFloat usLocale model.funds))
                ]
            ]
        , div []
            [ span []
                [ text ("Unsold Inventory: " ++ (toString model.inventory))
                ]
            ]
        , div []
            [ button [ onClick LowerPrice ] [ text "Lower" ]
            , button [ onClick RaisePrice ] [ text "Raise" ]
            , span []
                [ text
                    (" Price per Clip: $ "
                        ++ (formatFloat usLocale model.price)
                    )
                ]
            ]
        , div []
            [ span []
                [ text ("Public demand: " ++ (demandPercentage model.demand) ++ "%")
                ]
            ]
        , br [] []
        , div []
            [ button [ onClick BuyAds, disabled (model.funds < (toFloat model.marketingCost)) ] [ text "Marketing" ]
            , span []
                [ text (" Level: " ++ (toString model.marketingLvl))
                ]
            ]
        , div []
            [ text ("Cost: $ " ++ (formatInt usLocale model.marketingCost))
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


sellClips : BusinessModule -> Float -> BusinessModule
sellClips model rand =
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
            , marketingLvl = model.marketingLvl + 1
        }
