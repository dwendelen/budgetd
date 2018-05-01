module Page.Transactions.Update exposing (update)

import Model.Application
    exposing
        ( Model
        , goToOverview
        , newTransaction
        , changeDate
        , changeComment
        , changeBalance
        , deleteSubTransaction
        , duplicateSubTransaction
        )
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

        DeleteSubTransaction transactionId subTransactionId ->
            deleteSubTransaction transactionId subTransactionId model

        DuplicateSubTransaction transactionId subTransactionId ->
            duplicateSubTransaction transactionId subTransactionId model

        GoToOverview ->
            goToOverview model
