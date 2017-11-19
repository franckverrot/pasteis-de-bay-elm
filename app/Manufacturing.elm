module Manufacturing exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Utils exposing (demandPercentage)
import FormatNumber exposing (formatFloat, formatInt, usLocale)

import Business as Business

init : ManufacturingModule
init = {
    wires = 1000
    , wireSupply = 1000
    , wireCost = 15
    , wireBasePrice = 15
    , clipperModule = Nothing
    , megaClipperModule = Nothing
    , partialClips = 0
    , clipmakerRate = 0
    }

view : ManufacturingModule -> BusinessModule -> Html Msg
view model business =
    div [] [
        div [] [
            h2 [] [ text "Manufacturing" ]
        ]
        , div [] [
            span [] [
                text ("Clips per Second: " ++ (model.clipmakerRate |> round |> toString))
            ]
        ]
        , br [] []
        , div [] [
            button [ onClick BuyWires, disabled (business.funds < (toFloat model.wireCost)) ] [ text "Wire" ]
            , span [] [
                text ( " " ++ (toString model.wires) ++ " inches" )
            ]
        ]
        , div [] [
            text ( "Cost: $ " ++ (toString model.wireCost) )
        ]
        , clipperView model business
        , megaClipperView model business
    ]

clipperView : ManufacturingModule -> BusinessModule -> Html Msg
clipperView model business=
  case model.clipperModule of
    Nothing -> text ""
    Just mod ->
      div [] [
         br [] []
         , div [] [
             button [ onClick BuyClipper, disabled (business.funds < mod.cost) ] [ text "AutoClippers" ]
             , span [] [
                 text ( " " ++ (toString mod.level) )
             ]
         ]
         , div [] [
             text ( "Cost: $ " ++ (formatFloat usLocale mod.cost) )
         ]
      ]

megaClipperView : ManufacturingModule -> BusinessModule-> Html Msg
megaClipperView model business=
  case model.megaClipperModule of
    Nothing -> text ""
    Just mod ->
      div [] [
         br [] []
         , div [] [
             button [ onClick BuyMegaClipper, disabled (business.funds < mod.cost) ] [ text "MegaClippers" ]
             , span [] [
                 text ( " " ++ (toString mod.level) )
             ]
         ]
         , div [] [
             text ( "Cost: $ " ++ (formatFloat usLocale mod.cost) )
         ]
      ]

updateModel : ManufacturingModule -> BusinessModule -> ManufacturingModule
updateModel model business =
        { model |
        clipperModule = (tryMakeClipperModule model business.funds)
        , megaClipperModule = (tryMakeMegaClipperModule model)
        }

tryMakeClipperModule : ManufacturingModule -> Float -> Maybe ClipperModule
tryMakeClipperModule model funds =
  case model.clipperModule of
    Just mod ->
      let
        clipperCost = (1.1 ^ (toFloat mod.level)) + 4
      in
        Just {mod | cost = clipperCost}
    Nothing ->
      let
          enoughFunds = funds >= 5
      in
          case enoughFunds of
            False -> Nothing
            True -> Just { cost = 5
                         , boost = 1
                         , level = 0
                         }

tryMakeMegaClipperModule : ManufacturingModule -> Maybe MegaClipperModule
tryMakeMegaClipperModule model =
  case model.megaClipperModule of
    Just mod ->
      let
        megaClipperCost = (1.07 ^ (toFloat mod.level)) * 1000
      in
        Just {mod | cost = megaClipperCost}
    Nothing ->
      case model.clipperModule of
        Nothing -> Nothing
        Just clipperModule ->
          case clipperModule.level >= 75 of
            False -> Nothing
            True -> Just { cost = 1000
                         , boost = 1
                         , level = 0
                         }

addClipper : ManufacturingModule -> ManufacturingModule
addClipper model =
  case model.clipperModule of
        Nothing -> model
        Just mod -> {model | clipperModule = addClipper_ mod}

addClipper_ : ClipperModule -> Maybe ClipperModule
addClipper_ model =
    Just {model | level = model.level + 1}

addMegaClipper : ManufacturingModule -> ManufacturingModule
addMegaClipper model =
  case model.megaClipperModule of
        Nothing -> model
        Just mod -> {model | megaClipperModule = addMegaClipper_ mod}

addMegaClipper_ : MegaClipperModule -> Maybe MegaClipperModule
addMegaClipper_ model =
    Just {model | level = model.level + 1}

adjustwireCost : ManufacturingModule -> Float -> ManufacturingModule
adjustwireCost model rand =
    if (rand > 1.5) then
        model
    else
        let
            wireAdjust = 5 * (Basics.sin ((rand * 100) + 50))
        in
            { model
            | wireCost = ceiling (model.wireBasePrice + wireAdjust)
            }

createClip : Model -> Model
createClip model =
    if (model.manufacturingModule.wires < 1) then
        model
    else
        let
            businessModule = Business.addItems model.businessModule 1
            manufacturingModule = model.manufacturingModule
            newManufacturingModule = { manufacturingModule | wires = manufacturingModule.wires - 1 }
        in
            { model
           | clips = model.clips + 1
           , businessModule = businessModule
           , manufacturingModule = newManufacturingModule
           }

makeClips : Model -> Model
makeClips model =
    case model.manufacturingModule.wires of
        0 ->
          let
             manufacturingModule = model.manufacturingModule
             newManufacturingModule = { manufacturingModule
                     | clipmakerRate = 0
                     , partialClips = 0
                     }
          in
            {model
            | manufacturingModule = newManufacturingModule}
        _ ->
          let
              autoClipperAmount = runClippers model.manufacturingModule.clipperModule
              megaClipperAmount = runMegaClippers model.manufacturingModule.megaClipperModule
              partialClipsCapacity = model.manufacturingModule.partialClips + autoClipperAmount + megaClipperAmount
              fullClipsCapacity = floor partialClipsCapacity
              clipmakerRate = (Basics.min (autoClipperAmount + megaClipperAmount) (toFloat model.manufacturingModule.wires)) * 10
              fullClips = Basics.min fullClipsCapacity model.manufacturingModule.wires
              manufacturingModule = model.manufacturingModule
              newManufacturingModule = { manufacturingModule
                                    | partialClips = partialClipsCapacity - (toFloat fullClipsCapacity)
                                    , wires = model.manufacturingModule.wires - fullClips
                                    , clipmakerRate = clipmakerRate}
          in
              {
                  model
                  | clips = model.clips + fullClips
                  , businessModule = Business.addItems model.businessModule fullClips
                  , manufacturingModule = newManufacturingModule
              }

runClippers : Maybe ClipperModule -> Float
runClippers model =
  case model of
    Nothing -> 0
    Just mod -> (toFloat (mod.boost * mod.level)) / 10

runMegaClippers : Maybe MegaClipperModule -> Float
runMegaClippers model =
  case model of
    Nothing -> 0
    Just mod -> (toFloat (mod.boost * mod.level)) / 10

buyWires : ManufacturingModule -> BusinessModule -> (ManufacturingModule, BusinessModule)
buyWires model business =
    let
        wireCost = toFloat model.wireCost
    in
        if (business.funds < wireCost) then
            (model, business)
        else
            let
                businessModule = Business.removeFunds business wireCost
                manufacturingModule = { model
                       | wireBasePrice = model.wireBasePrice + 0.05
                       , wires = model.wires + model.wireSupply}
            in
                (manufacturingModule, businessModule)
