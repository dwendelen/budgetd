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


module Model.Limbo
    exposing
        ( getTransactionLimbo
        , getAccountLimbo
        , getBucketLimbo
        )

import Dict
import Model.Balance exposing (BalanceRef(..))
import Model.Transaction exposing (Amount, SubTransaction, TransactionId, TransactionList)
import Set


getAccountLimbo : TransactionList -> Amount
getAccountLimbo transactionList =
    let
        sumAllAmounts =
            getAllTransactions transactionList
                |> List.map (getTransactionLimbo transactionList)
                |> List.filter ((<) 0)
                -- All Greater then 0, or 0 > x
                |> List.sum
    in
        sumAllAmounts


getBucketLimbo : TransactionList -> Amount
getBucketLimbo transactionList =
    let
        sumAllAmounts =
            getAllTransactions transactionList
                |> List.map (getTransactionLimbo transactionList)
                |> List.filter ((>) 0)
                -- All Less then 0, or 0 < x
                |> List.sum
    in
        sumAllAmounts


getTransactionLimbo : TransactionList -> TransactionId -> Amount
getTransactionLimbo transactionList tId =
    let
        sumAllAmounts =
            transactionList.subTransactions
                |> Dict.values
                |> List.filter (\s -> s.transactionId == tId)
                |> List.map .amount
                |> List.sum
    in
        -1 * sumAllAmounts


getAllTransactions : TransactionList -> List TransactionId
getAllTransactions transactions =
    transactions.subTransactions
        |> Dict.values
        |> List.map .transactionId
        |> Set.fromList
        |> Set.toList
