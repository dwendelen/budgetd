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


module Model.Application
    exposing
        ( Model
        , Page(..)
        , initialModel
        , initCmd
        , subscriptions
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
        , createNewBucket
        , createNewAccount
        , handleEvent
        )

import Model.Balance exposing (..)
import Model.Limbo exposing (getTransactionLimbo)
import Model.Socket exposing (Event(..), Socket, initialMessage, send)
import Model.Transaction exposing (..)
import Page.Overview.Model exposing (PageState, initialState)


type alias Model =
    { page : Page
    , balances : BalanceList
    , transactions : TransactionList
    , socket : Socket
    , currentDate : String
    }


type Page
    = Overview PageState
    | TransactionsBalance BalanceRef
    | Error String


initCmd : Cmd msg
initCmd =
    initialMessage


initialModel : Model
initialModel =
    { page = Overview initialState
    , balances = Model.Balance.newBalanceList
    , transactions = Model.Transaction.initialTransactionList
    , socket = Model.Socket.initialSocket
    , currentDate = "0000-00-00"
    }


subscriptions : Sub Model.Socket.Msg
subscriptions =
    Model.Socket.subscriptions


goToOverview : Model -> Model
goToOverview model =
    { model | page = Overview Page.Overview.Model.initialState }


displayError : String -> Model -> Model
displayError errorMsg model =
    { model | page = Error errorMsg }


openTransactionsOfBalance : BalanceRef -> Model -> Model
openTransactionsOfBalance balanceRef model =
    { model | page = TransactionsBalance balanceRef }


createNewAccount : AccountId -> String -> Model -> ( Model, Cmd msg )
createNewAccount aId name model =
    executeEvent (CreateAccountEvent { accountId = aId, name = name }) model


createNewBucket : BucketId -> String -> Model -> ( Model, Cmd msg )
createNewBucket bId name model =
    executeEvent (CreateBucketEvent { bucketId = bId, name = name }) model


changeDate : SubTransactionId -> Date -> Model -> ( Model, Cmd msg )
changeDate sId newDate model =
    executeEvent (UpdateDateEvent { subTransactionId = sId, date = newDate }) model


changeComment : SubTransactionId -> Comment -> Model -> ( Model, Cmd msg )
changeComment sId newComment model =
    executeEvent (UpdateCommentEvent { subTransactionId = sId, comment = newComment }) model


changeBalance : SubTransactionId -> BalanceRef -> Model -> ( Model, Cmd msg )
changeBalance sId newBalanceRef model =
    executeEvent (UpdateBalanceEvent { subTransactionId = sId, balance = newBalanceRef }) model


changeAmount : SubTransactionId -> Amount -> Model -> ( Model, Cmd msg )
changeAmount sId newTransAmount model =
    executeEvent (UpdateAmountEvent { subTransactionId = sId, amount = newTransAmount }) model


deleteSubTransaction : SubTransactionId -> Model -> ( Model, Cmd msg )
deleteSubTransaction sId model =
    executeEvent (DeleteSubTransactionEvent sId) model


newSubTransaction : TransactionId -> BalanceRef -> Amount -> Model -> ( Model, Cmd msg )
newSubTransaction tId balanceRef amount model =
    let
        data =
            { subTransactionId = model.transactions.nextSubTransactionId
            , transactionId = tId
            , date = model.currentDate
            , balanceRef = balanceRef
            , comment = ""
            , amount = amount
            }
    in
        createSubTransaction_ data model


getTransactionLimbo : Model -> TransactionId -> Amount
getTransactionLimbo model tId =
    Model.Limbo.getTransactionLimbo model.transactions tId


newTransaction : BalanceRef -> Model -> ( Model, Cmd msg )
newTransaction balanceRef model =
    let
        creationData =
            { subTransactionId = model.transactions.nextSubTransactionId
            , transactionId = model.transactions.nextTransactionId
            , date = model.currentDate
            , balanceRef = balanceRef
            , amount = 0
            , comment = ""
            }
    in
        createSubTransaction_ creationData model


duplicateSubTransaction : SubTransactionId -> Model -> ( Model, Cmd msg )
duplicateSubTransaction sId model =
    subTransactionToData sId model
        |> Maybe.map (\data -> { data | subTransactionId = model.transactions.nextSubTransactionId })
        |> Maybe.map (\data -> createSubTransaction_ data model)
        |> Maybe.withDefault ( model, Cmd.none )


createSubTransaction_ : SubTransactionCreationData -> Model -> ( Model, Cmd msg )
createSubTransaction_ data model =
    let
        eventData =
            { subTransactionId = data.subTransactionId
            , transactionId = data.transactionId
            , date = data.date
            , balance = data.balanceRef
            , comment = data.comment
            , amount = data.amount
            }
    in
        executeEvent (CreateSubTransactionEvent eventData) model


subTransactionToData : SubTransactionId -> Model -> Maybe SubTransactionCreationData
subTransactionToData sId model =
    getSubTransaction sId model.transactions
        |> Maybe.map
            (\sub ->
                { subTransactionId = sId
                , transactionId = sub.transactionId
                , date = sub.date
                , balanceRef = sub.balanceRef
                , comment = sub.comment
                , amount = sub.amount
                }
            )


executeEvent : Event -> Model -> ( Model, Cmd msg )
executeEvent event model =
    let
        ( newSocket, cmd ) =
            send event model.socket

        model1 =
            { model | socket = newSocket }

        model2 =
            handleEvent event model1
    in
        ( model2, cmd )


handleEvent : Event -> Model -> Model
handleEvent event model =
    case Debug.log "event" event of
        CreateSubTransactionEvent createData ->
            let
                data =
                    { subTransactionId = createData.subTransactionId
                    , transactionId = createData.transactionId
                    , date = createData.date
                    , balanceRef = createData.balance
                    , comment = createData.comment
                    , amount = createData.amount
                    }
            in
                { model | transactions = createSubTransaction data model.transactions }

        UpdateDateEvent dateEventData ->
            { model | transactions = updateDate dateEventData.date dateEventData.subTransactionId model.transactions }

        UpdateCommentEvent commentEventData ->
            { model | transactions = updateComment commentEventData.comment commentEventData.subTransactionId model.transactions }

        UpdateBalanceEvent balanceEventData ->
            { model | transactions = updateBalance balanceEventData.balance balanceEventData.subTransactionId model.transactions }

        UpdateAmountEvent amountEventData ->
            { model | transactions = updateAmount amountEventData.amount amountEventData.subTransactionId model.transactions }

        DeleteSubTransactionEvent sId ->
            { model | transactions = Model.Transaction.deleteSubTransaction sId model.transactions }

        CreateBucketEvent createBucketEventData ->
            { model | balances = Model.Balance.createNewBucket createBucketEventData.bucketId createBucketEventData.name model.balances }

        CreateAccountEvent createAccountEventData ->
            { model | balances = Model.Balance.createNewAccount createAccountEventData.accountId createAccountEventData.name model.balances }

        ChangeRateEvent changeEventRateData ->
            model
