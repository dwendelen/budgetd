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
        , amountChanged
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
    , amount : Float
    }


type alias BucketId =
    Int


type alias Bucket =
    { id : BucketId
    , name : String
    , amount : Float
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
            nextAccountId balanceList

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


amountChanged : BalanceRef -> Float -> Float -> BalanceList -> BalanceList
amountChanged balanceRef amountFrom amountTo balanceList =
    case balanceRef of
        AccountRef accountId ->
            let
                transformer =
                    (\account ->
                        { account | amount = account.amount - amountFrom + amountTo }
                    )
            in
                updateAccount accountId transformer balanceList

        BucketRef bucketId ->
            let
                transformer =
                    (\bucket ->
                        { bucket | amount = bucket.amount - amountFrom + amountTo }
                    )
            in
                updateBucket bucketId transformer balanceList

        BufferRef ->
            let
                newBuffer =
                    balanceList.buffer - amountFrom + amountTo
            in
                { balanceList | buffer = newBuffer }

        NoBalanceRef ->
            balanceList


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
