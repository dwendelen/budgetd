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
import Model.Limbo exposing (getTransactionLimbo)
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
    , transactions = Model.Transaction.initialTransactionList
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
newTransaction balanceRef model =
    let
        ( transList1, subId ) =
            createTransaction model.transactions

        transList2 =
            updateBalance balanceRef subId transList1
    in
        { model | transactions = transList2 }


changeDate : SubTransactionId -> Date -> Model -> Model
changeDate sId newDate model =
    { model | transactions = updateDate newDate sId model.transactions }


changeComment : SubTransactionId -> Comment -> Model -> Model
changeComment sId newComment model =
    { model | transactions = updateComment newComment sId model.transactions }


changeBalance : SubTransactionId -> BalanceRef -> Model -> Model
changeBalance sId newBalanceRef model =
    { model | transactions = updateBalance newBalanceRef sId model.transactions }


changeAmount : SubTransactionId -> Amount -> Model -> Model
changeAmount sId newTransAmount model =
    { model | transactions = updateAmount newTransAmount sId model.transactions }


deleteSubTransaction : SubTransactionId -> Model -> Model
deleteSubTransaction sId model =
    { model | transactions = Model.Transaction.deleteSubTransaction sId model.transactions }


duplicateSubTransaction : SubTransactionId -> Model -> Model
duplicateSubTransaction sId model =
    { model | transactions = Model.Transaction.duplicateSubTransaction sId model.transactions }


newSubTransaction : TransactionId -> BalanceRef -> Amount -> Model -> Model
newSubTransaction tId balanceRef amount model =
    let
        ( trans1, sId ) =
            createSubTransaction tId model.transactions

        newTrans =
            trans1
                |> updateBalance balanceRef sId
                |> updateAmount amount sId
    in
        { model | transactions = newTrans }


getTransactionLimbo : Model -> TransactionId -> Amount
getTransactionLimbo model tId =
    Model.Limbo.getTransactionLimbo model.transactions tId
