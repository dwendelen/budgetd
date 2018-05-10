module Page.Transactions.Model
    exposing
        ( Msg(..)
        )

import Model.Balance exposing (BalanceRef)
import Model.Transaction exposing (..)


type Msg
    = NewTransaction BalanceRef
    | ChangeDate TransactionId SubTransactionId Date
    | ChangeComment TransactionId SubTransactionId Comment
    | ChangeBalance TransactionId SubTransactionId BalanceRef
    | ChangeAccountAmount TransactionId SubTransactionId String
    | ChangeBucketAmount TransactionId SubTransactionId String
    | DeleteSubTransaction TransactionId SubTransactionId
    | DuplicateSubTransaction TransactionId SubTransactionId
    | CreateSubTransactionFromLimbo Transaction BalanceRef
    | GoToOverview
