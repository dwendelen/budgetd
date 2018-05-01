module Model.Transaction
    exposing
        ( SubTransaction
        , SubTransactionId
        , Date
        , Comment
        , Transaction
        , TransactionId
        , TransactionList
        , newTransactionList
        , createNewTransaction
        , updateTransactionList
        , updateSubTransactions
        , updateDate
        , updateComment
        , updateBalance
        , deleteSubTransaction
        , duplicateSubTransaction
        , getLimbo
        )

import Model.Balance exposing (BalanceRef(AccountRef, BucketRef))


type alias TransactionList =
    { transactions : List Transaction
    , nextTransactionId : TransactionId
    }


type alias Transaction =
    { id : TransactionId
    , subTransactions : List SubTransaction
    , nextId : TransactionId
    }


type alias TransactionId =
    Int


type alias SubTransaction =
    { id : SubTransactionId
    , date : Date
    , balanceRef : BalanceRef
    , comment : Comment
    , amount : Amount
    }


type alias Date =
    String


type alias Comment =
    String


type alias Amount =
    Float


type alias SubTransactionId =
    Int


newTransactionList : TransactionList
newTransactionList =
    { transactions = []
    , nextTransactionId = 0
    }


createNewTransaction : TransactionList -> BalanceRef -> TransactionList
createNewTransaction transactionList parentId =
    let
        newTrans =
            newTransaction parentId transactionList.nextTransactionId

        newNextTransactionId =
            transactionList.nextTransactionId + 1
    in
        { transactionList
            | transactions = transactionList.transactions ++ [ newTrans ]
            , nextTransactionId = newNextTransactionId
        }


newTransaction : BalanceRef -> TransactionId -> Transaction
newTransaction idParent idNewTransaction =
    { id = idNewTransaction
    , subTransactions = [ newSubTransaction idParent ]
    , nextId = 1
    }


newSubTransaction : BalanceRef -> SubTransaction
newSubTransaction idParent =
    { id = 0
    , date = "2018-01-26"
    , balanceRef = idParent
    , comment = "Kei veel zever"
    , amount = 1500.0
    }


type alias TransactionTransformer =
    Transaction -> Maybe Transaction


updateTransactionList : TransactionId -> TransactionTransformer -> TransactionList -> TransactionList
updateTransactionList transactionId transformer transactionList =
    { transactionList
        | transactions = List.filterMap (updateTransaction transactionId transformer) transactionList.transactions
    }


updateTransaction : TransactionId -> TransactionTransformer -> Transaction -> Maybe Transaction
updateTransaction transactionId transformer transaction =
    if transaction.id == transactionId then
        transformer transaction
    else
        Just transaction


type alias SubTransactionTransformer =
    SubTransaction -> Maybe SubTransaction


updateSubTransactions : SubTransactionId -> SubTransactionTransformer -> Transaction -> Maybe Transaction
updateSubTransactions subTransactionId transformer transaction =
    let
        newSubTransactions =
            List.filterMap (updateSubTransaction subTransactionId transformer) transaction.subTransactions
    in
        if List.isEmpty newSubTransactions then
            Nothing
        else
            Just { transaction | subTransactions = newSubTransactions }


updateSubTransaction : SubTransactionId -> SubTransactionTransformer -> SubTransaction -> Maybe SubTransaction
updateSubTransaction subTransactionId transformer subTransaction =
    if subTransaction.id == subTransactionId then
        transformer subTransaction
    else
        Just subTransaction


updateDate : Date -> SubTransactionTransformer
updateDate newDate subTransaction =
    Just { subTransaction | date = newDate }


updateComment : Comment -> SubTransactionTransformer
updateComment newComment subTransaction =
    Just { subTransaction | comment = newComment }


updateBalance : BalanceRef -> SubTransactionTransformer
updateBalance newBalanceRef subTransaction =
    Just { subTransaction | balanceRef = newBalanceRef }


deleteSubTransaction : SubTransactionTransformer
deleteSubTransaction _ =
    Nothing


duplicateSubTransaction : SubTransactionId -> TransactionTransformer
duplicateSubTransaction subTransactionId transaction =
    let
        maybeNewSubTransaction =
            transaction.subTransactions
                |> List.filter (\s -> s.id == subTransactionId)
                |> List.head
                |> Maybe.map (\s -> { s | id = transaction.nextId })

        newNextId =
            transaction.nextId + 1
    in
        case maybeNewSubTransaction of
            Nothing ->
                Just transaction

            Just newSubTransaction ->
                Just
                    { transaction
                        | subTransactions = transaction.subTransactions ++ [ newSubTransaction ]
                        , nextId = newNextId
                    }


getLimbo : Transaction -> Amount
getLimbo transaction =
    let
        sumAllAmounts =
            transaction.subTransactions
                |> List.map (\st -> st.amount * (getSign st.balanceRef))
                |> List.sum
    in
        -1 * sumAllAmounts


getSign : BalanceRef -> Float
getSign balanceRef =
    case balanceRef of
        AccountRef _ ->
            1

        BucketRef _ ->
            -1
