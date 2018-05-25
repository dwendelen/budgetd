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


module Model.Balance
    exposing
        ( BalanceList
        , Account
        , AccountId
        , BalanceRef(..)
        , Bucket
        , BucketId
        , newBalanceList
        , createNewAccount
        , createNewBucket
        , nextAccountId
        , nextBucketId
        , getBufferLostValue
        , leakedInBucket
        )


import ISO8601
import Time exposing (Time)


type BalanceRef
    = AccountRef AccountId
    | BucketRef BucketId
    | BufferRef
    | NoBalanceRef


type alias BalanceList =
    { accounts : List Account
    , buckets : List Bucket
    , buffer : Float
    }


type alias AccountId =
    Int


type alias Account =
    { id : AccountId
    , name : String
    }


type alias BucketId =
    Int


type alias Bucket =
    { id : BucketId
    , name : String
    , rate : Float
    , time : Time
    , baseValue : Float
    }


newBalanceList : BalanceList
newBalanceList =
    { accounts = []
    , buckets = []
    , buffer = 0
    }


createNewAccount : AccountId -> String -> BalanceList -> BalanceList
createNewAccount newId name balanceList =
    let
        newAcc =
            newAccount newId name

        newAccs =
            balanceList.accounts ++ [ newAcc ]
    in
        { balanceList | accounts = newAccs }


createNewBucket : BucketId -> String -> BalanceList -> BalanceList
createNewBucket newId name balanceList =
    let
        newBuck =
            newBucket newId name

        newBucks =
            balanceList.buckets ++ [ newBuck ]
    in
        { balanceList | buckets = newBucks }


newAccount : AccountId -> String -> Account
newAccount accountId name =
    { id = accountId
    , name = name
    }


newBucket : BucketId -> String -> Bucket
newBucket bucketId name =
    { id = bucketId
    , name = name
    , rate = nbOfSecondsPerMonth * 0.1 --TODO
    , time = 1527171769588 --TODO
    , baseValue = 0
    }


nextAccountId : BalanceList -> AccountId
nextAccountId balanceList =
    List.map .id balanceList.accounts
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


nextBucketId : BalanceList -> BucketId
nextBucketId balanceList =
    List.map .id balanceList.buckets
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


updateBucket : BucketId -> (Bucket -> Bucket) -> BalanceList -> BalanceList
updateBucket bucketId transformer balanceList =
    let
        newBuckets =
            balanceList.buckets
                |> List.map
                    (\a ->
                        if a.id == bucketId then
                            transformer a
                        else
                            a
                    )
    in
        { balanceList | buckets = newBuckets }

nbOfSecondsPerMonth = 2628000


getBufferLostValue : BalanceList -> Time -> Float
getBufferLostValue balances time =
    let
        totalLeakage = balances.buckets
            |> List.map (leakedInBucket time)
            |> List.sum
    in
        -1 * totalLeakage


leakedInBucket : Time -> Bucket -> Float
leakedInBucket time bucket  =
    let
        delta = (time - bucket.time) / 1000 * bucket.rate / nbOfSecondsPerMonth
    in
        bucket.baseValue + delta