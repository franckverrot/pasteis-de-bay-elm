module Reactor exposing (..)

import Main as Main
import Models exposing (..)
import Html


main : Program Never Model Msg
main =
    Html.program
        { init = Main.init Nothing
        , view = Main.view
        , update = Main.update
        , subscriptions = Main.subscriptions
        }
