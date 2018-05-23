module Page.Transactions.Update exposing (update)

import Model.Application exposing (Model, changeAmount, changeBalance, changeComment, changeDate, deleteSubTransaction, duplicateSubTransaction, getTransactionLimbo, goToOverview, newSubTransaction, newTransaction)
import Page.Transactions.Model exposing (Msg(..))


update : Msg -> Model -> Model
update msg model =
    case Debug.log "msg" msg of
        NewTransaction parentId ->
            newTransaction parentId model

        ChangeDate subTransactionId newDate ->
            changeDate subTransactionId newDate model

        ChangeComment subTransactionId newComment ->
            changeComment subTransactionId newComment model

        ChangeBalance subTransactionId newBalanceId ->
            changeBalance subTransactionId newBalanceId model

        ChangeAccountAmount subTransactionId amountAsString ->
            changeAmount_ subTransactionId amountAsString 1.0 model

        ChangeBucketAmount subTransactionId amountAsString ->
            changeAmount_ subTransactionId amountAsString -1.0 model

        DeleteSubTransaction subTransactionId ->
            deleteSubTransaction subTransactionId model

        DuplicateSubTransaction subTransactionId ->
            duplicateSubTransaction subTransactionId model

        CreateSubTransactionFromLimbo tId balanceRef ->
            let
                amount =
                    getTransactionLimbo model tId
            in
                newSubTransaction tId balanceRef amount model

        GoToOverview ->
            goToOverview model


changeAmount_ subTransactionId amountAsString sign model =
    case String.toFloat <| String.trim <| amountAsString of
        Ok amount ->
            changeAmount subTransactionId (sign * amount) model

        _ ->
            model
