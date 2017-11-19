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
    , clips = saveModel.clips
    , businessModule = saveModel.businessModule
    , manufacturingModule = saveModel.manufacturingModule
    }


modelToSave : Model -> SaveModel
modelToSave model =
    { clips = model.clips
    , businessModule = model.businessModule
    , manufacturingModule = model.manufacturingModule
    }
