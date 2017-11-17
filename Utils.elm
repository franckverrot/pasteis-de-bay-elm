module Utils exposing (..)

import FormatNumber exposing (formatFloat, formatInt, usLocale)

demandPercentage: Float -> String
demandPercentage demand = formatFloat usLocale (demand * 10)