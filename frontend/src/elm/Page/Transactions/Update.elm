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


module Page.Transactions.Update exposing (update)

import Model.Application exposing (Model, changeAmount, changeBalance, changeComment, changeDate, deleteSubTransaction, duplicateSubTransaction, getTransactionLimbo, goToOverview, newSubTransaction, newTransaction)
import Model.Transaction exposing (SubTransactionId)
import Page.Transactions.Model exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case Debug.log "msg" msg of
        NewTransaction parentId ->
            newTransaction parentId model

        ChangeDate subTransactionId newDate ->
            changeDate subTransactionId newDate model

        ChangeComment subTransactionId newComment ->
            changeComment subTransactionId newComment model

        ChangeBalance subTransactionId newBalanceId ->
            changeBalance subTransactionId newBalanceId model

        ChangeAccountAmount subTransactionId amountAsString ->
            changeAmount_ subTransactionId amountAsString 1.0 model

        ChangeBucketAmount subTransactionId amountAsString ->
            changeAmount_ subTransactionId amountAsString -1.0 model

        DeleteSubTransaction subTransactionId ->
            deleteSubTransaction subTransactionId model

        DuplicateSubTransaction subTransactionId ->
            duplicateSubTransaction subTransactionId model

        CreateSubTransactionFromLimbo tId balanceRef ->
            let
                amount =
                    getTransactionLimbo model tId
            in
                newSubTransaction tId balanceRef amount model

        GoToOverview ->
            ( goToOverview model, Cmd.none )


changeAmount_ : SubTransactionId -> String -> Float -> Model -> ( Model, Cmd msg )
changeAmount_ subTransactionId amountAsString sign model =
    case String.toFloat <| String.trim <| amountAsString of
        Ok amount ->
            changeAmount subTransactionId (sign * amount) model

        _ ->
            ( model, Cmd.none )
