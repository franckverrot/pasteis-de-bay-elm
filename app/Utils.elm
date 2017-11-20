module Utils exposing (..)

import Material
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Models exposing (..)


demandPercentage : Float -> String
demandPercentage demand =
    formatFloat usLocale (demand * 10)


saveToModel : SaveModel -> Model
saveToModel saveModel =
    { mdl =
        Material.model
    , pasteis = saveModel.pasteis
    , businessModule = saveModel.businessModule
    , manufacturingModule = saveModel.manufacturingModule
    }


modelToSave : Model -> SaveModel
modelToSave model =
    { pasteis = model.pasteis
    , businessModule = model.businessModule
    , manufacturingModule = model.manufacturingModule
    }
