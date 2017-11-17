module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, every, second, millisecond)
import Random
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Models exposing (..)
import Utils exposing (..)
import Business exposing (businessView)

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
    funds = 0
    , clips = 0
    , inventory = 0
    , price = 0.25
    , wires = 1000
    , wirePrice = 15
    , wireOrderSize = 500
    , demand = 3
    , demandBoost = 1
    , marketingLvl = 1
    , marketingEffectiveness = 1
    }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CreateClip -> ({ model
            | clips = model.clips + 1, inventory = model.inventory + 1, wires = model.wires - 1}, Cmd.none)
        BuyWires ->
            let wirePrice = toFloat model.wirePrice
            in
                if (model.funds < wirePrice) then
                    (model, Cmd.none)
                else
                    ({ model | wires = model.wires + model.wireOrderSize, funds = model.funds - wirePrice}, Cmd.none)
        LowerPrice -> ({ model | price = (Basics.max (model.price - 0.01) 0.01)} |> updateModel, Cmd.none)
        RaisePrice -> ({ model | price = model.price + 0.01} |> updateModel, Cmd.none)
        Tick newTime -> ( model , Random.generate SellClips (Random.float 0 100) )
        SellClips rand -> (sellClips model rand, Cmd.none)
        UpdateModel -> (updateModel model, Cmd.none)


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
            button [ onClick CreateClip, disabled (model.wires < 1) ] [ text "Make a clip" ]
        ]
        , businessView model
        , div [] [
            div [] [
                h2 [] [ text "Manufacturing" ]
            ]
            , div [] [
                span [] [
                    text "Clips per Second: "
                ]
            ]
            , br [] []
            , div [] [
                button [ onClick BuyWires, disabled (model.funds < (toFloat model.wirePrice)) ] [ text "Wire" ]
                , span [] [
                    text ( " " ++ (toString model.wires) ++ " Inches" )
                ]
            ]
            , div [] [
                text ( "Cost: $ " ++ (toString model.wirePrice) )
            ]
        ]
    ]


updateModel : Model -> Model
updateModel model =
    let
        marketing = (1.1 ^ toFloat (model.marketingLvl - 1))
        demand = (( (0.8 / model.price) * marketing * toFloat model.marketingEffectiveness ) *  (toFloat model.demandBoost) * 1.1)
    in
        { model | demand = demand }


sellClips : Model -> Float -> Model
sellClips model rand =
    let demand = floor ( 0.7 * (model.demand ^ 1.15))
    in
        if (rand > model.demand) then
            model
        else if (demand > model.inventory) then
            { model | inventory = 0, funds = model.funds + (model.price * (toFloat model.inventory)) }
        else
            { model | inventory = model.inventory - demand, funds = model.funds + (model.price * (toFloat demand)) }
