module Model.Application
    exposing
        ( Model
        , Page(..)
        , initialModel
        , goToOverview
        , openTransactionsOfBalance
        , newTransaction
        , changeDate
        , changeBalance
        , changeComment
        , deleteSubTransaction
        , duplicateSubTransaction
        )

import Model.Balance exposing (..)
import Model.Transaction exposing (..)
import Page.Overview.Model exposing (PageState, initialState)


type alias Model =
    { page : Page
    , balances : BalanceList
    , transactions : TransactionList
    }


type Page
    = Overview PageState
    | TransactionsBalance BalanceRef
    | Error String


initialModel : Model
initialModel =
    { page = Overview initialState
    , balances = Model.Balance.newBalanceList
    , transactions = Model.Transaction.newTransactionList
    }


goToOverview : Model -> Model
goToOverview model =
    { model | page = Overview Page.Overview.Model.initialState }


displayError : String -> Model -> Model
displayError errorMsg model =
    { model | page = Error errorMsg }


openTransactionsOfBalance : BalanceRef -> Model -> Model
openTransactionsOfBalance balanceRef model =
    { model | page = TransactionsBalance balanceRef }


newTransaction : BalanceRef -> Model -> Model
newTransaction parentId model =
    { model | transactions = createNewTransaction model.transactions parentId }


changeDate : TransactionId -> SubTransactionId -> Date -> Model -> Model
changeDate transactionId subTransactionId newDate model =
    { model | transactions = updateTransactionList transactionId (updateSubTransactions subTransactionId (updateDate newDate)) model.transactions }


changeComment : TransactionId -> SubTransactionId -> Comment -> Model -> Model
changeComment transactionId subTransactionId newComment model =
    { model | transactions = updateTransactionList transactionId (updateSubTransactions subTransactionId (updateComment newComment)) model.transactions }


changeBalance : TransactionId -> SubTransactionId -> BalanceRef -> Model -> Model
changeBalance transactionId subTransactionId newBalanceId model =
    { model | transactions = updateTransactionList transactionId (updateSubTransactions subTransactionId (updateBalance newBalanceId)) model.transactions }


deleteSubTransaction : TransactionId -> SubTransactionId -> Model -> Model
deleteSubTransaction transactionId subTransactionId model =
    { model | transactions = updateTransactionList transactionId (updateSubTransactions subTransactionId Model.Transaction.deleteSubTransaction) model.transactions }


duplicateSubTransaction : TransactionId -> SubTransactionId -> Model -> Model
duplicateSubTransaction transactionId subTransactionId model =
    { model | transactions = updateTransactionList transactionId (Model.Transaction.duplicateSubTransaction subTransactionId) model.transactions }
