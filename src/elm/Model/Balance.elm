module Model.Balance
    exposing
        ( BalanceList
        , Account
        , AccountId
        , BalanceId
        , BalanceRef(..)
        , Bucket
        , BucketId
        , newBalanceList
        , createNewAccount
        , createNewBucket
        , getBalanceId
        )


type BalanceRef
    = AccountRef AccountId
    | BucketRef BucketId


type alias BalanceList =
    { accounts : List Account
    , buckets : List Bucket
    }


type alias BalanceId =
    Int


type alias AccountId =
    BalanceId


type alias Account =
    { id : AccountId
    , name : String
    , amount : Float
    }


type alias BucketId =
    BalanceId


type alias Bucket =
    { id : BucketId
    , name : String
    , amount : Float
    , rate : Float
    }


getBalanceId : BalanceRef -> BalanceId
getBalanceId balanceRef =
    case balanceRef of
        AccountRef accountId ->
            accountId

        BucketRef bucketId ->
            bucketId


newBalanceList : BalanceList
newBalanceList =
    { accounts = []
    , buckets = []
    }


createNewAccount : BalanceList -> BalanceList
createNewAccount balanceList =
    let
        newId =
            nextId balanceList

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
            nextId balanceList

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
    , amount = 0
    }


newBucket : BucketId -> Bucket
newBucket bucketId =
    { id = bucketId
    , name = "Bucket " ++ toString (bucketId + 1)
    , amount = 0
    , rate = 0
    }


nextId : BalanceList -> BalanceId
nextId balanceList =
    let
        accountIds =
            List.map .id balanceList.accounts

        bucketIds =
            List.map .id balanceList.buckets
    in
        accountIds
            ++ bucketIds
            |> List.maximum
            |> Maybe.map ((+) 1)
            |> Maybe.withDefault 0
