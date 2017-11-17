module Manufacturing exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Utils exposing (demandPercentage)
import FormatNumber exposing (formatFloat, formatInt, usLocale)

manufacturingView : Model -> Html Msg
manufacturingView model =
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
            button [ onClick BuyWires, disabled (model.funds < (toFloat model.wireCost)) ] [ text "Wire" ]
            , span [] [
                text ( " " ++ (toString model.wires) ++ " inches" )
            ]
        ]
        , div [] [
            text ( "Cost: $ " ++ (toString model.wireCost) )
        ]
        , clipperView model
    ]

clipperView : Model -> Html Msg
clipperView model =
  case model.clipperModule of
    Nothing -> text ""
    Just mod ->
      div [] [
         br [] []
         , div [] [
             button [ onClick BuyClipper, disabled (model.funds < mod.cost) ] [ text "AutoClippers" ]
             , span [] [
                 text ( " " ++ (toString model.clipmakerLevel) )
             ]
         ]
         , div [] [
             text ( "Cost: $ " ++ (formatFloat usLocale mod.cost) )
         ]
      ]
