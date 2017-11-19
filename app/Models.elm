module Models exposing (..)

import Material
import Time exposing (Time)


type alias BusinessModule =
    { funds : Float
    , inventory : Int
    , price : Float
    , demand : Float
    , demandBoost : Int
    , marketingCost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Int
    }


type alias ManufacturingModule =
    { wires : Int
    , wireSupply : Int
    , wireCost : Int
    , wireBasePrice : Float
    , clipperModule : Maybe ClipperModule
    , megaClipperModule : Maybe MegaClipperModule
    , partialClips : Float
    , clipmakerRate : Float
    }


type alias ClipperModule =
    { cost : Float
    , boost : Int
    , level : Int
    }


type alias MegaClipperModule =
    { cost : Float
    , boost : Int
    , level : Int
    }


type alias Model =
    { mdl : Material.Model
    , clips : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    }


type Msg
    = Mdl (Material.Msg Msg)
    | CreateClip
    | BuyWires
    | BuyAds
    | LowerPrice
    | RaisePrice
    | Tick Time
    | SellClips Float
    | AdjustwireCost Float
    | UpdateModel
    | BuyClipper
    | BuyMegaClipper
