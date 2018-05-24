{-
   Copyright 2018 Daan Wendelen
   Copyright 2018 Cegeka NV

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


module Page.Transactions.Page exposing (page)

import Dict
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Model.Application exposing (Model, getTransactionLimbo)
import Model.Balance exposing (BalanceRef(AccountRef, NoBalanceRef))
import Model.Limbo
import Model.Transaction exposing (Amount, Date, SubTransaction, SubTransactionId, TransactionId, TransactionList)
import Page.Transactions.Model exposing (..)
import View.BalancesDropDown exposing (BalancesDropdown, renderBalancesDropdown)


type alias InitialisedBalancesDropDown =
    OnSelection Msg -> BalancesDropdown Msg


type alias AdaptedBalancesDropDown =
    SubTransactionId -> BalancesDropdown Msg


type alias OnSelection msg =
    BalanceRef -> msg


page : Model -> BalanceRef -> Html Msg
page model balanceRef =
    let
        balancesDropdown =
            renderBalancesDropdown model.balances
    in
        div [ class "container_24" ] (renderTransactions balancesDropdown balanceRef model.transactions)


renderTransactions : InitialisedBalancesDropDown -> BalanceRef -> TransactionList -> List (Html Msg)
renderTransactions initialisedBalancesDropDown id transactions =
    let
        overview =
            div [ class "grid_24", onClick GoToOverview ] [ text "Overview" ]

        newTransaction =
            div [ class "grid_20 button suffix_4 newStuff", onClick (NewTransaction id) ] [ text "New Transaction" ]

        header =
            [ div [ class "alpha prefix_4 grid_4" ] [ text "Balance" ]
            , div [ class "grid_3 currency" ] [ text "Amount (A)" ]
            , div [ class "grid_3 currency" ] [ text "Amount (B)" ]
            , div [ class "prefix_2 grid_4 suffix_4 omega" ] [ text "Comment" ]
            ]

        items =
            transactions.subTransactions
                |> Dict.values
                |> List.filter (\sub -> sub.balanceRef == id)
                |> List.sortBy .date
                |> List.map (renderTransaction initialisedBalancesDropDown transactions)
    in
        overview :: newTransaction :: (header ++ items)


renderTransaction : InitialisedBalancesDropDown -> TransactionList -> SubTransaction -> Html Msg
renderTransaction initialisedBalancesDropDown transactions topSubTransaction =
    let
        adaptedBalancesDropDown =
            (\sId -> initialisedBalancesDropDown (ChangeBalance sId))

        subTransactions =
            let
                otherSubTransactions =
                    transactions.subTransactions
                        |> Dict.values
                        |> List.filter (\s -> s.transactionId == topSubTransaction.transactionId)
                        |> List.filter ((/=) topSubTransaction)
            in
                topSubTransaction
                    :: otherSubTransactions
                    |> List.concatMap (renderSubTransaction adaptedBalancesDropDown)

        limboTransaction =
            renderLimbo initialisedBalancesDropDown transactions topSubTransaction.transactionId
    in
        div [ class "alpha grid_24 transaction omega" ]
            (subTransactions ++ limboTransaction)


renderSubTransaction : AdaptedBalancesDropDown -> SubTransaction -> List (Html Msg)
renderSubTransaction balanceDropdown subTrans =
    [ input [ id <| renderId subTrans.id "date", class "alpha grid_4 niceInput", type_ "text", onInput (ChangeDate subTrans.id), value subTrans.date ] []
    , (balanceDropdown subTrans.id) subTrans.balanceRef
    , renderAmount (isAccount subTrans.balanceRef) subTrans.amount (ChangeAccountAmount subTrans.id)
    , renderAmount (not <| isAccount subTrans.balanceRef) (-1 * subTrans.amount) (ChangeBucketAmount subTrans.id)
    , input [ class "prefix_2 grid_4 niceInput", type_ "text", onInput (ChangeComment subTrans.id), value subTrans.comment ] []
    , div [ class "grid_1 button", onClick (DeleteSubTransaction subTrans.id) ] [ text "-" ]
    , div [ class "grid_1 button suffix_2 omega", onClick (DuplicateSubTransaction subTrans.id) ] [ text "+" ]
    ]


renderId : SubTransactionId -> String -> String
renderId subTransactionId suffix =
    toString subTransactionId ++ "_" ++ suffix


renderAmount : Bool -> Amount -> (String -> Msg) -> Html Msg
renderAmount isEditable amount action =
    if isEditable then
        input [ class "grid_3 niceInput currency", type_ "text", onInput action, value <| toString amount ] []
    else
        div [ class "grid_3 currency" ] [ text "..." ]


isAccount : BalanceRef -> Bool
isAccount reference =
    case reference of
        AccountRef _ ->
            True

        _ ->
            False


renderLimbo : InitialisedBalancesDropDown -> TransactionList -> TransactionId -> List (Html Msg)
renderLimbo initialisedBalancesDropDown transactions tId =
    let
        limboAmount =
            Model.Limbo.getTransactionLimbo transactions tId

        dropDown =
            initialisedBalancesDropDown (CreateSubTransactionFromLimbo tId)
    in
        if limboAmount == 0 then
            []
        else
            [ div [ class "alpha grid_4" ] [ text "..." ]
            , dropDown NoBalanceRef
            , div [ class "grid_3 currency" ]
                [ if limboAmount > 0 then
                    text <| toString limboAmount
                  else
                    text "..."
                ]
            , div [ class "grid_3 currency" ]
                [ if limboAmount < 0 then
                    text <| toString <| -1 * limboAmount
                  else
                    text "..."
                ]
            , div [ class "prefix_2 grid_4 suffix_4 omega" ] [ text "..." ]
            ]
