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


newSubTransaction : TransactionId -> BalanceRef -> Amount -> Model -> Model
newSubTransaction tId balanceRef amount model =
    let
        data =
            { transactionId = tId
            , date = "2018-05-01"
            , balanceRef = balanceRef
            , comment = ""
            , amount = amount
            }
    in
        createSubTransaction_ data model


getTransactionLimbo : Model -> TransactionId -> Amount
getTransactionLimbo model tId =
    Model.Limbo.getTransactionLimbo model.transactions tId


newTransaction : BalanceRef -> Model -> Model
newTransaction balanceRef model =
    let
        ( transList1, tId ) =
            popNextTransactionId model.transactions

        model1 = { model | transactions = transList1}

        creationData =
            { transactionId = tId
            , date = "2018-05-01"
            , balanceRef = balanceRef
            , amount = 0
            , comment = ""
            }

    in
        createSubTransaction_ creationData model1


duplicateSubTransaction : SubTransactionId -> Model -> Model
duplicateSubTransaction sId model =
    subTransactionToData sId model
        |> Maybe.map (\data -> createSubTransaction_ data model)
        |> Maybe.withDefault model


createSubTransaction_ : SubTransactionCreationData -> Model -> Model
createSubTransaction_ data model =
    { model | transactions = createSubTransaction data model.transactions }


subTransactionToData : SubTransactionId -> Model -> Maybe SubTransactionCreationData
subTransactionToData sId model =
    getSubTransaction sId model.transactions
        |> Maybe.map
            (\sub ->
                { transactionId = sub.transactionId
                , date = sub.date
                , balanceRef = sub.balanceRef
                , comment = sub.comment
                , amount = sub.amount
                }
            )
