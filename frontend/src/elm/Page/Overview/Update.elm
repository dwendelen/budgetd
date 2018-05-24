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
