module Page.Transactions.Model
    exposing
        ( Msg(..)
        )

import Model.Balance exposing (BalanceRef)
import Model.Transaction exposing (..)


type Msg
    = NewTransaction BalanceRef
    | ChangeDate SubTransactionId Date
    | ChangeComment SubTransactionId Comment
    | ChangeBalance SubTransactionId BalanceRef
    | ChangeAccountAmount SubTransactionId String
    | ChangeBucketAmount SubTransactionId String
    | DeleteSubTransaction SubTransactionId
    | DuplicateSubTransaction SubTransactionId
    | CreateSubTransactionFromLimbo TransactionId BalanceRef
    | GoToOverview
