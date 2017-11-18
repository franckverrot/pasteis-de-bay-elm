module Models exposing (..)

import Material
import Time exposing (Time)

type alias ClipperModule = {
    cost: Float
    , boost: Int
    , level: Int
    }

type alias MegaClipperModule = {
    cost: Float
    , boost: Int
    , level: Int
    }

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
    , clipperModule : Maybe ClipperModule
    , megaClipperModule : Maybe MegaClipperModule
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
    | BuyMegaClipper
