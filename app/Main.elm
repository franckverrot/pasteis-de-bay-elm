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
import Business as Business
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
    , businessModule = Business.init
    , clips = 0
    , partialClips = 0
    , wires = 1000
    , wireSupply = 1000
    , wireCost = 15
    , wireBasePrice = 15
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
            | clips = model.clips + 1
            , businessModule = Business.addItems model.businessModule 1
            , wires = model.wires - 1}, Cmd.none)
        BuyWires ->
            let
                wireCost = toFloat model.wireCost
            in
                if (model.businessModule.funds < wireCost) then
                    (model, Cmd.none)
                else
                    ({ model
                    | wires = model.wires + model.wireSupply
                    , businessModule = Business.removeFunds model.businessModule wireCost
                    , wireBasePrice = model.wireBasePrice + 0.05
                    }, Cmd.none)
        LowerPrice -> ({ model
            | businessModule = Business.lowerPrice model.businessModule}
            |> updateModel
            , Cmd.none
            )
        RaisePrice -> ({ model
            | businessModule = Business.lowerPrice model.businessModule}
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
        SellClips rand -> ({model
                            | businessModule = Business.sellClips model.businessModule rand
                            }, Cmd.none)
        AdjustwireCost rand -> (adjustwireCost model rand, Cmd.none)
        BuyClipper ->
          case model.clipperModule of
            Nothing -> (model, Cmd.none)
            Just mod ->
              { model | clipperModule = addClipper mod
                      , businessModule = Business.removeFunds model.businessModule mod.cost
              }
              |> flip (,) Cmd.none
        BuyMegaClipper ->
          case model.megaClipperModule of
            Nothing -> (model, Cmd.none)
            Just mod ->
              { model | megaClipperModule = addMegaClipper mod
                      , businessModule = Business.removeFunds model.businessModule mod.cost
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
        , Business.view model.businessModule
        , manufacturingView model
    ]
    |> Material.Scheme.top

tryMakeClipperModule : Model -> Maybe ClipperModule
tryMakeClipperModule model =
  case model.clipperModule of
    Just mod ->
      let
        clipperCost = (1.1 ^ (toFloat mod.level)) + 4
      in
        Just {mod | cost = clipperCost}
    Nothing ->
      let
          enoughFunds = model.businessModule.funds >= 5
      in
          case enoughFunds of
            False -> Nothing
            True -> Just { cost = 5
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
        { model |
        businessModule = Business.updateModel model.businessModule
        , clipperModule = (tryMakeClipperModule model)
        , megaClipperModule = (tryMakeMegaClipperModule model)
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
    case model.wires of
        0 -> { model
            | clipmakerRate = 0
            , partialClips = 0
            }
        _ ->
          let
              autoClipperAmount = runClippers model.clipperModule
              megaClipperAmount = runMegaClippers model.megaClipperModule
              partialClipsCapacity = model.partialClips + autoClipperAmount + megaClipperAmount
              fullClipsCapacity = floor partialClipsCapacity
              clipmakerRate = (Basics.min (autoClipperAmount + megaClipperAmount) (toFloat model.wires)) * 10
              fullClips = Basics.min fullClipsCapacity model.wires
          in
              {
                  model
                  | partialClips = partialClipsCapacity - (toFloat fullClipsCapacity)
                  , businessModule = Business.addItems model.businessModule fullClips
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