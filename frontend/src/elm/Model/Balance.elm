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
        )


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
    }


newBalanceList : BalanceList
newBalanceList =
    { accounts = []
    , buckets = []
    , buffer = 0
    }


createNewAccount : BalanceList -> BalanceList
createNewAccount balanceList =
    let
        newId =
            nextAccountId balanceList

        newAcc =
            newAccount newId

        newAccs =
            balanceList.accounts ++ [ newAcc ]
    in
        { balanceList | accounts = newAccs }


createNewBucket : BalanceList -> BalanceList
createNewBucket balanceList =
    let
        newId =
            nextBucketId balanceList

        newBuck =
            newBucket newId

        newBucks =
            balanceList.buckets ++ [ newBuck ]
    in
        { balanceList | buckets = newBucks }


newAccount : AccountId -> Account
newAccount accountId =
    { id = accountId
    , name = "Account " ++ toString (accountId + 1)
    }


newBucket : BucketId -> Bucket
newBucket bucketId =
    { id = bucketId
    , name = "Bucket " ++ toString (bucketId + 1)
    , rate = 0
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


updateAccount : AccountId -> (Account -> Account) -> BalanceList -> BalanceList
updateAccount accountId transformer balanceList =
    let
        newAccounts =
            balanceList.accounts
                |> List.map
                    (\a ->
                        if a.id == accountId then
                            transformer a
                        else
                            a
                    )
    in
        { balanceList | accounts = newAccounts }


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
