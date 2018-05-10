module Model.Limbo
    exposing
        ( Limbo
        , newLimbo
        , getTransactionLimbo
        , amountChanged
        )

import Model.Balance exposing (BalanceRef)
import Model.Transaction exposing (Amount, Transaction, TransactionId)


type alias Limbo =
    { accountLimbo : Float
    , bucketLimbo : Float
    }


newLimbo : Limbo
newLimbo =
    { accountLimbo = 0
    , bucketLimbo = 0
    }


amountChanged : Transaction -> Amount -> Amount -> Limbo -> Limbo
amountChanged transaction amountFrom amountTo limbo =
    let
        oldLimbo =
            getTransactionLimbo transaction

        newLimbo =
            oldLimbo + amountFrom - amountTo

        newAccountLimbo =
            limbo.accountLimbo - (toAccountLimbo oldLimbo) + (toAccountLimbo newLimbo)

        newBucketLimbo =
            limbo.bucketLimbo - (toBucketLimbo oldLimbo) + (toBucketLimbo newLimbo)
    in
        { accountLimbo = newAccountLimbo
        , bucketLimbo = newBucketLimbo
        }


toAccountLimbo : Float -> Float
toAccountLimbo limbo =
    max 0 limbo


toBucketLimbo : Float -> Float
toBucketLimbo limbo =
    min 0 limbo


getTransactionLimbo : Transaction -> Amount
getTransactionLimbo transaction =
    let
        sumAllAmounts =
            transaction.subTransactions
                |> List.map .amount
                |> List.sum
    in
        -1 * sumAllAmounts
