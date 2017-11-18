module Models exposing (..)

import Material
import Time exposing (Time)

type alias BusinessModule = {
    funds : Float
    , inventory : Int
    , price : Float
    , demand : Float
    , demandBoost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Int
    }

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
    , businessModule : BusinessModule
    , clips : Int
    , partialClips : Float
    , wires : Int
    , wireSupply : Int
    , wireCost : Int
    , wireBasePrice : Float
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
