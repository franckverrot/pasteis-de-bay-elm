module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options
import Time exposing (Time, every, second, millisecond)
import Random
import FormatNumber exposing (formatFloat, formatInt, usLocale)


import Models exposing (..)
import Utils exposing (..)
import Business exposing (..)
import Manufacturing exposing (..)


main : Program Never Model Msg
main = 
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : (Model, Cmd Msg)
init = (updateModel {
    mdl = Material.model
    , funds = 0
    , clips = 0
    , partialClips = 0
    , inventory = 0
    , price = 0.25
    , wires = 1000
    , wireSupply = 1000
    , wireCost = 15
    , wireBasePrice = 15
    , demand = 3
    , demandBoost = 1
    , marketingLvl = 1
    , marketingEffectiveness = 1
    , clipperModule = Nothing
    , megaClipperModule = Nothing
    , clipmakerRate = 0
    }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CreateClip -> ({ model
            | clips = model.clips + 1, inventory = model.inventory + 1, wires = model.wires - 1}, Cmd.none)
        BuyWires ->
            let
                wireCost = toFloat model.wireCost
            in
                if (model.funds < wireCost) then
                    (model, Cmd.none)
                else
                    ({ model
                    | wires = model.wires + model.wireSupply
                    , funds = model.funds - wireCost
                    , wireBasePrice = model.wireBasePrice + 0.05
                    }, Cmd.none)
        LowerPrice -> ({ model
            | price = (Basics.max (model.price - 0.01) 0.01)}
            |> updateModel
            , Cmd.none
            )
        RaisePrice -> ({ model
            | price = model.price + 0.01}
            |> updateModel
            , Cmd.none
            )
        Tick newTime -> ( model
            |> makeClips
            |> updateModel
            , Cmd.batch [
                Random.generate SellClips (Random.float 0 100)
                , Random.generate AdjustwireCost (Random.float 0 100)
             ]
            )
        SellClips rand -> (sellClips model rand, Cmd.none)
        AdjustwireCost rand -> (adjustwireCost model rand, Cmd.none)
        BuyClipper ->
          case model.clipperModule of
            Nothing -> (model, Cmd.none)
            Just mod ->
              { model | clipperModule = addClipper mod
                      , funds = model.funds - mod.cost
              }
              |> flip (,) Cmd.none
        BuyMegaClipper ->
          case model.megaClipperModule of
            Nothing -> (model, Cmd.none)
            Just mod ->
              { model | megaClipperModule = addMegaClipper mod
                      , funds = model.funds - mod.cost
              }
              |> flip (,) Cmd.none
        UpdateModel -> (updateModel model, Cmd.none)
        Mdl msg_ ->
            Material.update Mdl msg_ model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        every (100 * millisecond) Tick
    ]


view : Model -> Html Msg
view model =
    div [] [
        h1 [] [
            text ( "Clips " ++ (formatInt usLocale model.clips) )
            ]
        , div [] [
            Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Button.raised
                        , Button.ripple
                        , Options.onClick CreateClip
                        , Options.disabled (model.wires < 1)
                        ]
                        [ text "Make a clip" ]
        ]
        , businessView model
        , manufacturingView model
    ]
    |> Material.Scheme.top

tryMakeClipperModule : Model -> Maybe ClipperModule
tryMakeClipperModule model =
  case model.clipperModule of
    Just mod ->
      let
        clipperCost = (1.1 ^ (toFloat mod.level)) + 5
      in
        Just {mod | cost = clipperCost}
    Nothing ->
      let
          enoughFunds = model.funds >= 5
      in
          case enoughFunds of
            False -> Nothing
            True -> Just { cost = 5.0
                         , boost = 1
                         , level = 0
                         }

tryMakeMegaClipperModule : Model -> Maybe MegaClipperModule
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

addClipper : ClipperModule -> Maybe ClipperModule
addClipper model =
    Just {model | level = model.level + 1}

addMegaClipper : MegaClipperModule -> Maybe MegaClipperModule
addMegaClipper model =
    Just {model | level = model.level + 1}

updateModel : Model -> Model
updateModel model =
    let
        marketing = (1.1 ^ toFloat (model.marketingLvl - 1))
        demand = (( (0.8 / model.price) * marketing * toFloat model.marketingEffectiveness ) *  (toFloat model.demandBoost) * 1.1)
    in
        { model |
        demand = demand
        , clipperModule = (tryMakeClipperModule model)
        , megaClipperModule = (tryMakeMegaClipperModule model)
        }


sellClips : Model -> Float -> Model
sellClips model rand =
    let
        demand = floor ( 0.7 * (model.demand ^ 1.15))
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

adjustwireCost : Model -> Float -> Model
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


makeClips : Model -> Model
makeClips model =
  let
      autoClipperAmount = runClippers model.clipperModule
      megaClipperAmount = runMegaClippers model.megaClipperModule
      partalClips = model.partialClips + autoClipperAmount + megaClipperAmount
      fullClips = Basics.min (floor partalClips) model.wires
      clipmakerRate = (autoClipperAmount + megaClipperAmount) * 10
  in
      {
          model
          | partialClips = partalClips - (toFloat fullClips)
          , inventory = model.inventory + fullClips
          , clips = model.clips + fullClips
          , wires = model.wires - fullClips
          , clipmakerRate = clipmakerRate
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