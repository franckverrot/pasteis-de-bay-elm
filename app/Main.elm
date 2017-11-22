port module Main exposing (..)

import Html exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, Device(..), Align(..), align)
import Time exposing (Time, every, second, millisecond)
import Random
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Models exposing (..)
import Utils exposing (..)
import Business as Business
import Manufacturing as Manufacturing
import Computing as Computing


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
    , lastTick = Nothing
    , pasteis = 0
    , businessModule = Business.init
    , manufacturingModule = Manufacturing.init
    , computingModule = Nothing
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
            ( applyTime model newTime
            , Cmd.batch
                [ saveState (Utils.modelToSave model)
                ]
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

        AddProcessor ->
            case model.computingModule of
                Nothing ->
                    ( model, Cmd.none )

                Just computingModule ->
                    { model
                        | computingModule = Just (Computing.addProcessor computingModule)
                    }
                        |> flip (,) Cmd.none

        AddMemory ->
            case model.computingModule of
                Nothing ->
                    ( model, Cmd.none )

                Just computingModule ->
                    { model
                        | computingModule = Just (Computing.addMemory computingModule)
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
        , grid []
            [ cell [ size All 3 ]
                [ Options.div
                    [ css "display" "flex"
                    , css "flex-flow" "row wrap"
                    , css "align-items" "flex-end"
                    , css "margin-top" "20px"
                    ]
                    [ Options.div
                        [ css "display" "flex"
                        , css "flex-flow" "row wrap"
                        , css "justify-content" "space-between"
                        , css "align-items" "center"
                        , css "min-width" "256px"
                        , css "flex" "1 1 auto"
                        ]
                        [ Business.view model
                        , Manufacturing.view model
                        ]
                    ]
                ]
            , cell [ size All 3 ]
                [ Options.div
                    [ css "display" "flex"
                    , css "flex-flow" "row wrap"
                    , css "align-items" "flex-end"
                    , css "margin-top" "20px"
                    ]
                    [ Options.div
                        [ css "display" "flex"
                        , css "flex-flow" "row wrap"
                        , css "justify-content" "space-between"
                        , css "align-items" "center"
                        , css "min-width" "256px"
                        , css "flex" "1 1 auto"
                        ]
                        [ Computing.view model
                        ]
                    ]
                ]
            , cell [ size All 3 ]
                [ Options.div
                    [ css "display" "flex"
                    , css "flex-flow" "row wrap"
                    , css "align-items" "flex-end"
                    , css "margin-top" "20px"
                    ]
                    [ Options.div
                        [ css "display" "flex"
                        , css "flex-flow" "row wrap"
                        , css "justify-content" "space-between"
                        , css "align-items" "center"
                        , css "min-width" "256px"
                        , css "flex" "1 1 auto"
                        ]
                        []
                    ]
                ]
            ]
        ]
        |> Material.Scheme.top


updateModel : Model -> Model
updateModel model =
    let
        businessModule =
            Business.updateModel model.businessModule

        manufacturingModule =
            Manufacturing.updateModel model.manufacturingModule businessModule

        computingModule =
            Computing.updateModel model
    in
        { model
            | businessModule = businessModule
            , manufacturingModule = manufacturingModule
        }
            |> Computing.tryMakeComputingModule


applyTime : Model -> Time -> Model
applyTime model time =
    case model.lastTick of
        Nothing ->
            let
                seed0 =
                    Random.initialSeed (floor (Time.inMilliseconds time))

                ( float1, seed1 ) =
                    Utils.randomFloat 0 100 seed0

                ( float2, seed2 ) =
                    Utils.randomFloat 0 100 seed1
            in
                applyTime_ model ( [ float1 ], [ float2 ] ) |> flip setLastTick time

        Just lastTick ->
            let
                elapsedTime =
                    (Time.inMilliseconds time) - (Time.inMilliseconds lastTick)

                operationsToRun =
                    Basics.min (Basics.max (floor (elapsedTime / 100)) 1) 3000

                seed0 =
                    Random.initialSeed (floor (Time.inMilliseconds time))

                ( floatList1, seed1 ) =
                    Utils.randomMultipleFloat 0 100 operationsToRun seed0

                ( floatList2, seed2 ) =
                    Utils.randomMultipleFloat 0 100 operationsToRun seed1
            in
                applyTime_ model ( floatList1, floatList1 ) |> flip setLastTick time


applyTime_ : Model -> ( List Float, List Float ) -> Model
applyTime_ model ( floatList, floatList2 ) =
    case List.length floatList of
        0 ->
            model

        _ ->
            let
                float1 =
                    Maybe.withDefault 0 (List.head floatList)

                float2 =
                    Maybe.withDefault 0 (List.head floatList2)

                floats1 =
                    Maybe.withDefault [] (List.tail floatList)

                floats2 =
                    Maybe.withDefault [] (List.tail floatList2)
            in
                { model
                    | businessModule = Business.sellPasteis model.businessModule float1
                    , manufacturingModule = Manufacturing.adjustdoughCost model.manufacturingModule float2
                }
                    |> Manufacturing.makePasteis
                    |> makeOperations
                    |> updateModel
                    |> flip applyTime_ ( floats1, floats2 )


makeOperations : Model -> Model
makeOperations model =
    case model.computingModule of
        Nothing ->
            model

        Just mod ->
            { model | computingModule = Just (Computing.makeOperations mod) }


setLastTick : Model -> Time -> Model
setLastTick model time =
    { model | lastTick = Just time }
