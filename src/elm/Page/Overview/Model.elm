module Page.Overview.Model
    exposing
        ( PageState
        , initialState
        , Msg(..)
        )

import Model.Balance exposing (BalanceId, BalanceRef)


type alias PageState =
    { editing : Maybe BalanceRef
    }


type Msg
    = StartEditingBalanceName BalanceRef
    | StopEditingBalanceName
    | OpenTransactionsBalance BalanceRef
    | NewAccount
    | NewBucket


initialState : PageState
initialState =
    { editing = Nothing
    }
