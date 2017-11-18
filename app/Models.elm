module Models exposing (..)

import Material
import Time exposing (Time)

type alias Model = {
    mdl : Material.Model
    , funds : Float
    , clips : Int
    , partialClips : Float
    , inventory : Int
    , price : Float
    , wires : Int
    , wireSupply : Int
    , wireCost : Int
    , wireBasePrice : Float
    , demand : Float
    , demandBoost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Int
    , clipperActivated : Bool
    , clipperCost : Float
    , clipperBoost : Int
    , clipmakerLevel : Int
    , megaClipperActivated : Bool
    , megaClipperBoost : Int
    , megaClipperLevel : Int
    , megaClipperCost : Float
    , clipmakerRate: Float
    }


type Msg =
    Mdl (Material.Msg Msg)
    | CreateClip
    | BuyWires
    | LowerPrice
    | RaisePrice
    | Tick Time
    | SellClips Float
    | AdjustwireCost Float
    | UpdateModel
    | BuyClipper
