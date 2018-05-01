module Page.Transactions.Model
    exposing
        ( Msg(..)
        )

import Model.Balance exposing (BalanceId, BalanceRef)
import Model.Transaction exposing (..)


type Msg
    = NewTransaction BalanceRef
    | ChangeDate TransactionId SubTransactionId Date
    | ChangeComment TransactionId SubTransactionId Comment
    | ChangeBalance TransactionId SubTransactionId BalanceRef
    | DeleteSubTransaction TransactionId SubTransactionId
    | DuplicateSubTransaction TransactionId SubTransactionId
    | GoToOverview
