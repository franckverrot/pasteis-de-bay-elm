module Business exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Utils exposing (demandPercentage)
import FormatNumber exposing (formatFloat, formatInt, usLocale)

businessView : Model -> Html Msg
businessView model = div [] [
         div [] [
             h2 [] [ text "Business" ]
         ]
         , div [] [
             span [] [
                 text ( "Available Funds: $ " ++ ( formatFloat usLocale model.funds ))
             ]
         ]
--        , div [] [
--            span [] [
--                text ( "Avg. Rev. per sec: $ " ++ (toString model.clips) )
--            ]
--        ]
--        , div [] [
--            span [] [
--                text ( "Avg. Clips Sold per sec: " ++ (toString model.clips) )
--            ]
--        ]
         , div [] [
             span [] [
                 text ( "Unsold Inventory: " ++ (toString model.inventory) )
             ]
         ]
         , div [] [
             button [ onClick LowerPrice ] [ text "Lower" ]
             , button [ onClick RaisePrice ] [ text "Raise" ]
             , span [] [
                 text ( " Price per Clip: $ "
                     ++ ( formatFloat usLocale model.price )
                     )
             ]
         ]
         , div [] [
             span [] [
                 text ( "Public demand: " ++ (demandPercentage model.demand) ++ "%")
             ]
         ]
     ]