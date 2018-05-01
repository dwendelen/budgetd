module View.BalancesDropDown
    exposing
        ( BalancesDropdown
        , renderBalancesDropdown
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
    select [ class "grid_4 niceInput", onInput (onSelection << stringToBalanceRef) ] (renderBalancesDropdownOptions balances (getBalanceId selectedRef))


renderBalancesDropdownOptions : BalanceList -> BalanceId -> List (Html msg)
renderBalancesDropdownOptions balances selectedId =
    (renderAccountOptions balances.accounts selectedId) ++ (renderBucketOptions balances.buckets selectedId)


renderAccountOptions : List Account -> BalanceId -> List (Html msg)
renderAccountOptions accounts selectedId =
    List.map (renderAccountOption selectedId) accounts


renderBucketOptions : List Bucket -> BalanceId -> List (Html msg)
renderBucketOptions buckets selectedId =
    List.map (renderBucketOption selectedId) buckets


renderAccountOption : BalanceId -> Account -> Html msg
renderAccountOption selectedId account =
    option [ value <| "a" ++ toString account.id, selected (account.id == selectedId) ] [ text account.name ]


renderBucketOption : BalanceId -> Bucket -> Html msg
renderBucketOption selectedId bucket =
    option [ value <| "b" ++ toString bucket.id, selected (bucket.id == selectedId) ] [ text bucket.name ]


stringToBalanceRef : String -> BalanceRef
stringToBalanceRef input =
    case uncons input of
        Just ( 'a', number ) ->
            AccountRef (stringToBalanceId number)

        Just ( 'b', number ) ->
            BucketRef (stringToBalanceId number)

        _ ->
            Debug.crash "Could not map string to balance ref because the format is wrong"


stringToBalanceId : String -> BalanceId
stringToBalanceId input =
    case String.toInt input of
        Ok balanceId ->
            balanceId

        Err msg ->
            Debug.crash msg
