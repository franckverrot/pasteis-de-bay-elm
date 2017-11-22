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


type alias ComputingModule =
    { trust : Int
    , processors : Int
    , memory : Int
    , memoryLimit : Int
    , operations : Float
    , creativityEnable : Bool
    , creativity : Float
    }


type alias SaveModel =
    { pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    , computingModule : Maybe ComputingModule
    }


type alias Model =
    { mdl : Material.Model
    , lastTick : Maybe Time
    , pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    , computingModule : Maybe ComputingModule
    }


type Msg
    = Mdl (Material.Msg Msg)
    | CreatePastel
    | BuyDough
    | BuyAds
    | LowerPrice
    | RaisePrice
    | Tick Time
    | UpdateModel
    | BuyPasteis
    | BuyMegaPasteis
    | AddProcessor
    | AddMemory
