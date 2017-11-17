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
                text ( " " ++ (toString model.wires) ++ " Inches" )
            ]
        ]
        , div [] [
            text ( "Cost: $ " ++ (toString model.wireCost) )
        ]
        , (clipper model) |> Maybe.withDefault (text "")
    ]

clipper : Model -> Maybe (Html Msg)
clipper model =
    if (model.clipperActivated) then
        Just (div [] [
            br [] []
            , div [] [
                button [ onClick BuyClipper, disabled (model.funds < model.clipperCost) ] [ text "AutoClippers" ]
                , span [] [
                    text ( " " ++ (toString model.clipmakerLevel) )
                ]
            ]
            , div [] [
                text ( "Cost: $ " ++ (formatFloat usLocale model.clipperCost) )
            ]
        ])
    else
        Nothing