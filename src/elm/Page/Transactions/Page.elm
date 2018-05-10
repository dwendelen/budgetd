module Page.Transactions.Page exposing (page)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Model.Application exposing (Model, getTransactionLimbo)
import Model.Balance exposing (BalanceRef(AccountRef, NoBalanceRef))
import Model.Transaction exposing (Amount, Date, SubTransaction, SubTransactionId, Transaction, TransactionId)
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
        div [ class "container_24" ] (renderTransactions balancesDropdown balanceRef model.transactions.transactions)


renderTransactions : InitialisedBalancesDropDown -> BalanceRef -> List Transaction -> List (Html Msg)
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
            transactions
                |> List.filter (\t -> List.any (\st -> st.balanceRef == id) t.subTransactions)
                |> List.map (\t -> renderTransaction initialisedBalancesDropDown t)
    in
        overview :: newTransaction :: (header ++ items)


renderTransaction : InitialisedBalancesDropDown -> Transaction -> Html Msg
renderTransaction initialisedBalancesDropDown transaction =
    let
        adaptedBalancesDropDown =
            (\sId -> initialisedBalancesDropDown (ChangeBalance transaction.id sId))

        subTransactions =
            transaction.subTransactions
                |> List.concatMap (renderSubTransaction adaptedBalancesDropDown transaction.id)

        limboTransaction =
            renderLimbo initialisedBalancesDropDown transaction
    in
        div [ class "alpha grid_24 transaction omega" ]
            (subTransactions ++ limboTransaction)


renderSubTransaction : AdaptedBalancesDropDown -> TransactionId -> SubTransaction -> List (Html Msg)
renderSubTransaction balanceDropdown transId subTrans =
    [ input [ class "alpha grid_4 niceInput", type_ "text", onInput (ChangeDate transId subTrans.id), value subTrans.date ] []
    , (balanceDropdown subTrans.id) subTrans.balanceRef
    , renderAmount (isAccount subTrans.balanceRef) subTrans.amount (ChangeAccountAmount transId subTrans.id)
    , renderAmount (not <| isAccount subTrans.balanceRef) (-1 * subTrans.amount) (ChangeBucketAmount transId subTrans.id)
    , input [ class "prefix_2 grid_4 niceInput", type_ "text", onInput (ChangeComment transId subTrans.id), value subTrans.comment ] []
    , div [ class "grid_1 button", onClick (DeleteSubTransaction transId subTrans.id) ] [ text "-" ]
    , div [ class "grid_1 button suffix_2 omega", onClick (DuplicateSubTransaction transId subTrans.id) ] [ text "+" ]
    ]


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


renderLimbo : InitialisedBalancesDropDown -> Transaction -> List (Html Msg)
renderLimbo initialisedBalancesDropDown transaction =
    let
        limboAmount =
            getTransactionLimbo transaction

        dropDown =
            initialisedBalancesDropDown (CreateSubTransactionFromLimbo transaction)
    in
        if limboAmount == 0 then
            []
        else
            [ div [ class "alpha grid_4" ] [ text "..." ]
            , dropDown NoBalanceRef
            , div [ class "grid_3 currency" ] [ text "..." ]
            , div [ class "grid_3 currency" ] [ text <| toString <| -1 * limboAmount ]
            , div [ class "prefix_2 grid_4 suffix_4 omega" ] [ text "..." ]
            ]
