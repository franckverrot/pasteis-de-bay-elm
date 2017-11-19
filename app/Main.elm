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
import Manufacturing as Manufacturing


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
    , clips = 0
    , businessModule = Business.init
    , manufacturingModule = Manufacturing.init
    }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CreateClip -> (Manufacturing.createClip model, Cmd.none)
        BuyWires ->
            let
                (manufacturingModule, businessModule) = Manufacturing.buyWires model.manufacturingModule model.businessModule
            in
                ({ model
                | businessModule = businessModule
                , manufacturingModule = manufacturingModule
                }, Cmd.none)
        LowerPrice -> ({ model
            | businessModule = Business.lowerPrice model.businessModule}
            |> updateModel
            , Cmd.none
            )
        RaisePrice -> ({ model
            | businessModule = Business.raisePrice model.businessModule}
            |> updateModel
            , Cmd.none
            )
        Tick newTime -> ( model
            |> Manufacturing.makeClips
            |> updateModel
            , Cmd.batch [
                Random.generate SellClips (Random.float 0 100)
                , Random.generate AdjustwireCost (Random.float 0 100)
             ]
            )
        SellClips rand -> ({model
                            | businessModule = Business.sellClips model.businessModule rand
                            }, Cmd.none)
        AdjustwireCost rand -> ({model
                            | manufacturingModule = Manufacturing.adjustwireCost model.manufacturingModule rand
                            }
                            , Cmd.none)
        BuyClipper ->
          case model.manufacturingModule.clipperModule of
            Nothing -> (model, Cmd.none)
            Just mod ->
              { model
              | businessModule = Business.removeFunds model.businessModule mod.cost
              , manufacturingModule = Manufacturing.addClipper model.manufacturingModule
              }
              |> flip (,) Cmd.none
        BuyMegaClipper ->
          case model.manufacturingModule.megaClipperModule of
            Nothing -> (model, Cmd.none)
            Just mod ->
              { model
              | businessModule = Business.removeFunds model.businessModule mod.cost
              , manufacturingModule = Manufacturing.addMegaClipper model.manufacturingModule
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
                        , Options.disabled (model.manufacturingModule.wires < 1)
                        ]
                        [ text "Make a clip" ]
        ]
        , Business.view model.businessModule
        , Manufacturing.view model.manufacturingModule model.businessModule
    ]
    |> Material.Scheme.top

updateModel : Model -> Model
updateModel model =
    let
        businessModule = Business.updateModel model.businessModule
        manufacturingModule = Manufacturing.updateModel model.manufacturingModule businessModule
    in
        { model |
        businessModule = businessModule
        , manufacturingModule = manufacturingModule
        }
