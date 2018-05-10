module Page.Transactions.Update exposing (update)

import Model.Application exposing (Model, changeAmount, changeBalance, changeComment, changeDate, deleteSubTransaction, duplicateSubTransaction, getTransactionLimbo, goToOverview, newSubTransaction, newTransaction)
import Page.Transactions.Model exposing (Msg(..))


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewTransaction parentId ->
            newTransaction parentId model

        ChangeDate transactionId subTransactionId newDate ->
            changeDate transactionId subTransactionId newDate model

        ChangeComment transactionId subTransactionId newComment ->
            changeComment transactionId subTransactionId newComment model

        ChangeBalance transactionId subTransactionId newBalanceId ->
            changeBalance transactionId subTransactionId newBalanceId model
                |> Result.withDefault model

        ChangeAccountAmount transactionId subTransactionId amountAsString ->
            changeAmount_ transactionId subTransactionId amountAsString 1.0 model

        ChangeBucketAmount transactionId subTransactionId amountAsString ->
            changeAmount_ transactionId subTransactionId amountAsString -1.0 model

        DeleteSubTransaction transactionId subTransactionId ->
            deleteSubTransaction transactionId subTransactionId model

        DuplicateSubTransaction transactionId subTransactionId ->
            duplicateSubTransaction transactionId subTransactionId model

        CreateSubTransactionFromLimbo transaction balanceRef ->
            let
                amount =
                    getTransactionLimbo transaction
            in
                newSubTransaction transaction.id balanceRef amount model

        GoToOverview ->
            goToOverview model


changeAmount_ transactionId subTransactionId amountAsString sign model =
    case Debug.log "amount" (String.toFloat <| String.trim <| amountAsString) of
        Ok amount ->
            changeAmount transactionId subTransactionId (sign * amount) model
                |> Result.withDefault model

        _ ->
            model
