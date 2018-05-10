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
        , changeAmount
        , deleteSubTransaction
        , duplicateSubTransaction
        , newSubTransaction
        , getTransactionLimbo
        )

import Model.Balance exposing (..)
import Model.Limbo
    exposing
        ( Limbo
        , getTransactionLimbo
        )
import Model.Transaction exposing (..)
import Page.Overview.Model exposing (PageState, initialState)


type alias Model =
    { page : Page
    , balances : BalanceList
    , transactions : TransactionList
    , limbo : Limbo
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
    , limbo = Model.Limbo.newLimbo
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


changeBalance : TransactionId -> SubTransactionId -> BalanceRef -> Model -> Result String Model
changeBalance transactionId subTransactionId newBalanceRef model =
    getTransaction transactionId model
        |> Result.andThen
            (\transaction ->
                getSubTransaction subTransactionId transaction
                    |> Result.map
                        (\subtransaction ->
                            let
                                oldBalance =
                                    subtransaction.balanceRef

                                amount =
                                    subtransaction.amount

                                newTransactions =
                                    updateTransactionList transactionId (updateSubTransactions subTransactionId (updateBalance newBalanceRef)) model.transactions

                                newBalances =
                                    model.balances
                                        |> Model.Balance.amountChanged oldBalance amount 0
                                        |> Model.Balance.amountChanged newBalanceRef 0 amount
                            in
                                { model
                                    | transactions = newTransactions
                                    , balances = newBalances
                                }
                        )
            )


changeAmount : TransactionId -> SubTransactionId -> Amount -> Model -> Result String Model
changeAmount transactionId subTransactionId newTransAmount model =
    getTransaction transactionId model
        |> Result.andThen
            (\transaction ->
                getSubTransaction subTransactionId transaction
                    |> Result.map
                        (\subtransaction ->
                            let
                                oldTransAmount =
                                    subtransaction.amount

                                balanceRef =
                                    subtransaction.balanceRef

                                newTransactionList =
                                    updateTransactionList transactionId (updateSubTransactions subTransactionId (updateAmount newTransAmount)) model.transactions

                                newLimbo =
                                    Model.Limbo.amountChanged transaction oldTransAmount newTransAmount model.limbo

                                newBalances =
                                    Model.Balance.amountChanged balanceRef oldTransAmount newTransAmount model.balances
                            in
                                { model
                                    | transactions = newTransactionList
                                    , balances = newBalances
                                    , limbo = newLimbo
                                }
                        )
            )


deleteSubTransaction : TransactionId -> SubTransactionId -> Model -> Model
deleteSubTransaction transactionId subTransactionId model =
    { model | transactions = updateTransactionList transactionId (updateSubTransactions subTransactionId Model.Transaction.deleteSubTransaction) model.transactions }


duplicateSubTransaction : TransactionId -> SubTransactionId -> Model -> Model
duplicateSubTransaction transactionId subTransactionId model =
    { model | transactions = updateTransactionList transactionId (Model.Transaction.duplicateSubTransaction subTransactionId) model.transactions }


newSubTransaction : TransactionId -> BalanceRef -> Amount -> Model -> Model
newSubTransaction transactionId balanceRef amount model =
    { model | transactions = updateTransactionList transactionId (Model.Transaction.createNewSubTransaction balanceRef amount) model.transactions }


getTransactionLimbo : Transaction -> Amount
getTransactionLimbo transaction =
    Model.Limbo.getTransactionLimbo transaction


getTransaction : TransactionId -> Model -> Result String Transaction
getTransaction transId model =
    let
        maybeTransaction =
            model.transactions.transactions
                |> List.filter (\t -> t.id == transId)
                |> List.head
    in
        Result.fromMaybe ("Could not find transaction " ++ toString transId) maybeTransaction


getSubTransaction : SubTransactionId -> Transaction -> Result String SubTransaction
getSubTransaction subId transaction =
    let
        maybeTransaction =
            transaction.subTransactions
                |> List.filter (\t -> t.id == subId)
                |> List.head
    in
        Result.fromMaybe ("Could not find subtransaction " ++ toString subId) maybeTransaction

