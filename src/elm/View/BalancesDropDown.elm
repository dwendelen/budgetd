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
