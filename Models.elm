module Models exposing (..)

import Time exposing (Time)

type alias Model = {
    funds: Float
    , clips : Int
    , inventory : Int
    , price : Float
    , wires : Int
    , wirePrice : Int
    , wireOrderSize : Int
    , demand : Float
    , demandBoost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Int
    }


type Msg =
    CreateClip
    | BuyWires
    | LowerPrice
    | RaisePrice
    | Tick Time
    | SellClips Float
    | UpdateModel