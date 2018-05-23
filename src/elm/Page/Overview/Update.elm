module Page.Overview.Update exposing (update)

import Model.Application exposing (Model, Page(Overview), openTransactionsOfBalance)
import Model.Balance exposing (createNewAccount, createNewBucket)
import Page.Overview.Model exposing (Msg(..), PageState)


update : PageState -> Model -> Msg -> Model
update state model msg =
    case msg of
        StartEditingBalanceName balanceId ->
            --{ model | page = Overview { state | editing = Just balanceId } }
            model

        StopEditingBalanceName ->
            { model | page = Overview { state | editing = Nothing } }

        OpenTransactionsBalance balanceRef ->
            openTransactionsOfBalance balanceRef model

        NewAccount ->
            { model | balances = createNewAccount model.balances }

        NewBucket ->
            { model | balances = createNewBucket model.balances }
