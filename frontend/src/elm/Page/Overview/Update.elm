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

import Model.Application exposing (Model, Page(Overview), createNewAccount, createNewBucket, openTransactionsOfBalance)
import Model.Balance exposing (nextAccountId, nextBucketId)
import Page.Overview.Model exposing (Msg(..), PageState)


update : PageState -> Model -> Msg -> (Model, Cmd msg)
update state model msg =
    case msg of
        StartEditingBalanceName balanceId ->
            --{ model | page = Overview { state | editing = Just balanceId } }
            (model, Cmd.none)

        StopEditingBalanceName ->
            ({ model | page = Overview { state | editing = Nothing }}, Cmd.none)

        OpenTransactionsBalance balanceRef ->
            (openTransactionsOfBalance balanceRef model, Cmd.none)

        NewAccount ->
            let
                newId =
                    nextAccountId model.balances

                name =
                    "Account " ++ toString (newId + 1)
            in
                createNewAccount newId name model

        NewBucket ->
            let
                newId =
                    nextBucketId model.balances

                name =
                    "Bucket " ++ toString (newId + 1)
            in
                    createNewBucket newId name model

