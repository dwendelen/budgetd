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


module View.BalancesDropDown
    exposing
        ( BalancesDropdown
        , renderBalancesDropdown
        , stringToBalanceRef
        , balanceRefToString
        )

import Html exposing (Html, option, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Model.Balance exposing (..)
import String exposing (uncons)


type alias BalancesDropdown msg =
    BalanceRef -> Html msg


type alias OnSelection msg =
    BalanceRef -> msg


renderBalancesDropdown : BalanceList -> OnSelection msg -> BalancesDropdown msg
renderBalancesDropdown balances onSelection selectedRef =
    select [ class "grid_4 niceInput", onInput (onSelection << stringToBalanceRef) ] (renderBalancesDropdownOptions balances selectedRef)


renderBalancesDropdownOptions : BalanceList -> BalanceRef -> List (Html msg)
renderBalancesDropdownOptions balances selectedRef =
    let
        limbo =
            renderLimboOption selectedRef

        accounts =
            renderAccountOptions balances.accounts selectedRef

        buffer =
            renderBufferOption selectedRef

        buckets =
            renderBucketOptions balances.buckets selectedRef
    in
        [ limbo ] ++ accounts ++ [ buffer ] ++ buckets


renderLimboOption : BalanceRef -> Html msg
renderLimboOption selectedRef =
    option [ value <| "limbo", selected (NoBalanceRef == selectedRef) ] [ text "Limbo" ]


renderAccountOptions : List Account -> BalanceRef -> List (Html msg)
renderAccountOptions accounts selectedRef =
    List.map (renderAccountOption selectedRef) accounts


renderAccountOption : BalanceRef -> Account -> Html msg
renderAccountOption selectedRef account =
    option [ value <| "a" ++ toString account.id, selected (AccountRef account.id == selectedRef) ] [ text account.name ]


renderBufferOption : BalanceRef -> Html msg
renderBufferOption selectedRef =
    option [ value <| "buffer", selected (BufferRef == selectedRef) ] [ text "Buffer" ]


renderBucketOptions : List Bucket -> BalanceRef -> List (Html msg)
renderBucketOptions buckets selectedRef =
    List.map (renderBucketOption selectedRef) buckets


renderBucketOption : BalanceRef -> Bucket -> Html msg
renderBucketOption selectedRef bucket =
    option [ value <| "b" ++ toString bucket.id, selected (BucketRef bucket.id == selectedRef) ] [ text bucket.name ]


stringToBalanceRef : String -> BalanceRef
stringToBalanceRef input =
    case input of
        "buffer" ->
            BufferRef

        "limbo" ->
            NoBalanceRef

        _ ->
            accountOrBucketToBalanceRef input


balanceRefToString : BalanceRef -> String
balanceRefToString balanceRef =
    case balanceRef of
        NoBalanceRef ->
            "limbo"

        BufferRef ->
            "buffer"

        AccountRef aId ->
            "a" ++ toString aId

        BucketRef bId ->
            "b" ++ toString bId


accountOrBucketToBalanceRef : String -> BalanceRef
accountOrBucketToBalanceRef input =
    case uncons input of
        Just ( 'a', number ) ->
            AccountRef (stringToBalanceId number)

        Just ( 'b', number ) ->
            BucketRef (stringToBalanceId number)

        _ ->
            Debug.crash "Could not map string to balance ref because the format is wrong"


stringToBalanceId : String -> Int
stringToBalanceId input =
    case String.toInt input of
        Ok balanceId ->
            balanceId

        Err msg ->
            Debug.crash msg
