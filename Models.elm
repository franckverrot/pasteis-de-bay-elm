module Models exposing (..)

import Time exposing (Time)

type alias Model = {
    funds: Float
    , clips : Int
    , partialClips : Float
    , inventory : Int
    , price : Float
    , wires : Int
    , wireCost : Int
    , wireBasePrice : Float
    , wireOrderSize : Int
    , demand : Float
    , demandBoost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Int
    , clipperActivated : Bool
    , clipperCost : Float
    , clipperBoost : Int
    , clipmakerLevel : Int
    , megaClipperBoost : Int
    , megaClipperLevel : Int
    , megaClipperCost : Float
    , clipmakerRate: Float
    }


type Msg =
    CreateClip
    | BuyWires
    | LowerPrice
    | RaisePrice
    | Tick Time
    | SellClips Float
    | AdjustwireCost Float
    | UpdateModel
    | BuyClipper
