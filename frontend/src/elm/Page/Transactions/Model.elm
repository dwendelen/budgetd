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


module Page.Transactions.Model exposing (Msg(..))

import Model.Balance exposing (BalanceRef)
import Model.Transaction exposing (..)


type Msg
    = NewTransaction BalanceRef
    | ChangeDate SubTransactionId Date
    | ChangeComment SubTransactionId Comment
    | ChangeBalance SubTransactionId BalanceRef
    | ChangeAccountAmount SubTransactionId String
    | ChangeBucketAmount SubTransactionId String
    | DeleteSubTransaction SubTransactionId
    | DuplicateSubTransaction SubTransactionId
    | CreateSubTransactionFromLimbo TransactionId BalanceRef
    | GoToOverview
