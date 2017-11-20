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
    { dough : Int
    , doughSupply : Int
    , doughCost : Int
    , doughBasePrice : Float
    , pasteisModule : Maybe PasteisModule
    , megaPasteisModule : Maybe MegaPasteisModule
    , partialPasteis : Float
    , pasteisMakerRate : Float
    }


type alias PasteisModule =
    { cost : Float
    , boost : Int
    , level : Int
    }


type alias MegaPasteisModule =
    { cost : Float
    , boost : Int
    , level : Int
    }


type alias SaveModel =
    { pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    }


type alias Model =
    { mdl : Material.Model
    , pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    }


type Msg
    = Mdl (Material.Msg Msg)
    | CreatePastel
    | BuyDough
    | BuyAds
    | LowerPrice
    | RaisePrice
    | Tick Time
    | SellPasteis Float
    | AdjustdoughCost Float
    | UpdateModel
    | BuyPasteis
    | BuyMegaPasteis
