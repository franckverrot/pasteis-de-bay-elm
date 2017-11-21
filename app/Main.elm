port module Main exposing (..)

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


main : Program (Maybe SaveModel) Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


port saveState : SaveModel -> Cmd msg


emptyModel : Model
emptyModel =
    { mdl = Material.model
    , pasteis = 0
    , businessModule = Business.init
    , manufacturingModule = Manufacturing.init
    }


init : Maybe SaveModel -> ( Model, Cmd Msg )
init savedModel =
    case savedModel of
        Nothing ->
            ( emptyModel, Cmd.none )

        Just mod ->
            ( updateModel (Utils.saveToModel mod), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreatePastel ->
            ( Manufacturing.createPastel model, Cmd.none )

        BuyDough ->
            let
                ( manufacturingModule, businessModule ) =
                    Manufacturing.buyDough model.manufacturingModule model.businessModule
            in
                ( { model
                    | businessModule = businessModule
                    , manufacturingModule = manufacturingModule
                  }
                , Cmd.none
                )

        LowerPrice ->
            ( { model
                | businessModule = Business.lowerPrice model.businessModule
              }
                |> updateModel
            , Cmd.none
            )

        RaisePrice ->
            ( { model
                | businessModule = Business.raisePrice model.businessModule
              }
                |> updateModel
            , Cmd.none
            )

        BuyAds ->
            ( { model
                | businessModule = Business.buyAds model.businessModule
              }
            , Cmd.none
            )

        Tick newTime ->
            ( model
                |> Manufacturing.makePasteis
                |> updateModel
            , Cmd.batch
                [ Random.generate SellPasteis (Random.float 0 100)
                , Random.generate AdjustdoughCost (Random.float 0 100)
                , saveState (Utils.modelToSave model)
                ]
            )

        SellPasteis rand ->
            ( { model
                | businessModule = Business.sellPasteis model.businessModule rand
              }
            , Cmd.none
            )

        AdjustdoughCost rand ->
            ( { model
                | manufacturingModule = Manufacturing.adjustdoughCost model.manufacturingModule rand
              }
            , Cmd.none
            )

        BuyPasteis ->
            case model.manufacturingModule.pasteisModule of
                Nothing ->
                    ( model, Cmd.none )

                Just mod ->
                    { model
                        | businessModule = Business.removeFunds model.businessModule mod.cost
                        , manufacturingModule = Manufacturing.addPasteis model.manufacturingModule
                    }
                        |> flip (,) Cmd.none

        BuyMegaPasteis ->
            case model.manufacturingModule.megaPasteisModule of
                Nothing ->
                    ( model, Cmd.none )

                Just mod ->
                    { model
                        | businessModule = Business.removeFunds model.businessModule mod.cost
                        , manufacturingModule = Manufacturing.addMegaPasteis model.manufacturingModule
                    }
                        |> flip (,) Cmd.none

        UpdateModel ->
            ( updateModel model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every (100 * millisecond) Tick
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text ("Pasteis " ++ (formatInt usLocale model.pasteis))
            ]
        , div []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Options.onClick CreatePastel
                , Options.disabled (model.manufacturingModule.dough < 1)
                ]
                [ text "Make a Pastel" ]
            ]
        , Business.view model.businessModule
        , Manufacturing.view model
        ]
        |> Material.Scheme.top


updateModel : Model -> Model
updateModel model =
    let
        businessModule =
            Business.updateModel model.businessModule

        manufacturingModule =
            Manufacturing.updateModel model.manufacturingModule businessModule
    in
        { model
            | businessModule = businessModule
            , manufacturingModule = manufacturingModule
        }
