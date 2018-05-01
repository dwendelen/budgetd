module Page.Transactions.Page exposing (page)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Model.Application exposing (Model)
import Model.Balance exposing (BalanceId, BalanceRef)
import Model.Transaction exposing (SubTransaction, SubTransactionId, Transaction, TransactionId, getLimbo)
import Page.Transactions.Model exposing (..)
import View.BalancesDropDown exposing (BalancesDropdown, renderBalancesDropdown)


type alias AdaptedBalancesDropDown =
    TransactionId -> SubTransactionId -> BalancesDropdown Msg


page : Model -> BalanceRef -> Html Msg
page model balanceRef =
    let
        balancesDropdown =
            (\tId sId -> renderBalancesDropdown model.balances (ChangeBalance tId sId))
    in
        div [ class "container_24" ] (renderTransactions balancesDropdown balanceRef model.transactions.transactions)


renderTransactions : AdaptedBalancesDropDown -> BalanceRef -> List Transaction -> List (Html Msg)
renderTransactions balancesDropdown id transactions =
    let
        overview =
            div [ class "grid_24", onClick GoToOverview ] [ text "Overview" ]

        newTransaction =
            div [ class "grid_20 button suffix_4", onClick (NewTransaction id) ] [ text "New Transaction" ]

        items =
            transactions
                |> List.filter (\t -> List.any (\st -> st.balanceRef == id) t.subTransactions)
                |> List.map (\t -> renderTransaction balancesDropdown t)
    in
        overview :: newTransaction :: items


renderTransaction : AdaptedBalancesDropDown -> Transaction -> Html Msg
renderTransaction balancesDropdown transaction =
    div [ class "alpha grid_24 transaction omega" ]
        ((transaction.subTransactions
            |> List.concatMap (renderSubTransaction balancesDropdown transaction.id)
         )
            ++ renderLimbo transaction
        )


renderSubTransaction : AdaptedBalancesDropDown -> TransactionId -> SubTransaction -> List (Html Msg)
renderSubTransaction balanceDropdown transId subTrans =
    [ input [ class "alpha grid_4 niceInput", type_ "text", onInput (ChangeDate transId subTrans.id), value subTrans.date ] []
    , (balanceDropdown transId subTrans.id) subTrans.balanceRef
    , input [ class "grid_4 niceInput", type_ "text", onInput (ChangeComment transId subTrans.id), value subTrans.comment ] []
    , input [ class "grid_4 niceInput currency", type_ "text", value (toString subTrans.amount) ] []
    , input [ class "grid_4 niceInput currency", type_ "text", value "000000" ] []
    , div [ class "grid_1 button", onClick (DeleteSubTransaction transId subTrans.id) ] [ text "-" ]
    , div [ class "grid_1 button suffix_2 omega", onClick (DuplicateSubTransaction transId subTrans.id) ] [ text "+" ]
    ]


renderLimbo : Transaction -> List (Html Msg)
renderLimbo transaction =
    let
        limboAmount =
            getLimbo transaction
    in
        if limboAmount == 0 then
            []
        else
            [ div [ class "alpha grid_4" ] [ text "a" ]
            , div [ class "grid_4" ] [ text "Limbo" ]
            , div [ class "grid_4" ] [ text "a" ]
            , div [ class "grid_4 currency" ] [ text <| toString <| limboAmount ]
            , div [ class "grid_4 currency suffix_4 omega" ] [ text "000000" ]
            ]
