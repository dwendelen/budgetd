module Model.Transaction
    exposing
        ( TransactionList
        , SubTransaction
        , SubTransactionId
        , TransactionId
        , Date
        , Comment
        , Amount
        , initialTransactionList
        , createSubTransaction
        , createTransaction
        , updateDate
        , updateComment
        , updateBalance
        , updateAmount
        , deleteSubTransaction
        , getSubTransaction
        , duplicateSubTransaction
        , getAmount
        )

import Dict exposing (Dict)
import Model.Balance exposing (BalanceRef(..))


type alias TransactionList =
    { subTransactions : Dict SubTransactionId SubTransaction
    , nextTransactionId : TransactionId
    , nextSubTransactionId : SubTransactionId
    }


type alias TransactionId =
    Int


type alias SubTransaction =
    { id : SubTransactionId
    , transactionId: TransactionId
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


initialTransactionList : TransactionList
initialTransactionList =
    { subTransactions = Dict.empty
    , nextSubTransactionId = 0
    , nextTransactionId = 0
    }

createSubTransaction : TransactionId -> TransactionList -> (TransactionList, SubTransactionId)
createSubTransaction transactionId transactionList =
    let
        newSubTrans =
            initialSubTransaction transactionList.nextSubTransactionId transactionId
        newTransactionList =
            { transactionList
                        | subTransactions = Dict.insert newSubTrans.id newSubTrans transactionList.subTransactions
                        , nextSubTransactionId = transactionList.nextSubTransactionId + 1
                    }
    in
        (newTransactionList, transactionList.nextSubTransactionId)

initialSubTransaction : SubTransactionId -> TransactionId -> SubTransaction
initialSubTransaction subTransactionId transactionId =
    { id = subTransactionId
    , transactionId = transactionId
    , date = "2018-01-26"
    , balanceRef = NoBalanceRef
    , comment = ""
    , amount = 0
    }

createTransaction : TransactionList -> (TransactionList, SubTransactionId)
createTransaction transactionList =
    let
        (transactionListAfterNewSub, newSubId) =
            createSubTransaction transactionList.nextTransactionId transactionList
        newTransactionList =
            { transactionListAfterNewSub
                | nextTransactionId = transactionList.nextTransactionId + 1
                }
    in
        (newTransactionList, newSubId)

updateDate : Date -> SubTransactionId -> TransactionList -> TransactionList
updateDate newDate subId transactionList =
    apply (\subTransaction ->  { subTransaction | date = newDate }) subId transactionList


updateComment : Comment -> SubTransactionId -> TransactionList -> TransactionList
updateComment newComment subId transactionList =
    apply (\subTransaction ->  { subTransaction | comment = newComment }) subId transactionList


updateBalance : BalanceRef -> SubTransactionId -> TransactionList -> TransactionList
updateBalance newBalanceRef subId transactionList =
    apply (\subTransaction ->  { subTransaction | balanceRef = newBalanceRef }) subId transactionList


updateAmount : Amount -> SubTransactionId -> TransactionList -> TransactionList
updateAmount newAmount subId transactionList =
    apply (\subTransaction -> { subTransaction | amount = newAmount }) subId transactionList


apply : (SubTransaction -> SubTransaction) -> SubTransactionId -> TransactionList -> TransactionList
apply transformer sId transactionList =
    let
        newList = transactionList.subTransactions
            |> Dict.update sId (Maybe.map transformer)
    in
        { transactionList
            | subTransactions = newList
            }

duplicateSubTransaction : SubTransactionId -> TransactionList -> TransactionList
duplicateSubTransaction baseSubId transactionList =
    getSubTransaction baseSubId transactionList
        |> Maybe.map (\baseSub ->
            let
                newSub = {baseSub | id = transactionList.nextSubTransactionId}
                newNextSubId = transactionList.nextSubTransactionId + 1
            in
                {transactionList
                | nextSubTransactionId = newNextSubId
                , subTransactions = Dict.insert newSub.id newSub transactionList.subTransactions
                }
        )
        |> Maybe.withDefault transactionList

deleteSubTransaction :  SubTransactionId -> TransactionList -> TransactionList
deleteSubTransaction sId transactionList =
    let
        newList = transactionList.subTransactions
            |> Dict.remove sId
    in
        { transactionList
            | subTransactions = newList
            }
getSubTransaction : SubTransactionId -> TransactionList -> Maybe SubTransaction
getSubTransaction sId transactionList =
    transactionList.subTransactions
        |> Dict.get sId

getAmount: BalanceRef -> TransactionList -> Amount
getAmount balanceRef transactions =
    transactions.subTransactions
        |> Dict.values
        |> List.filter (\sub -> sub.balanceRef == balanceRef)
        |> List.map .amount
        |> List.sum