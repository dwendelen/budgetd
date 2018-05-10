module Page.Overview.Page exposing (page)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick)
import Model.Application exposing (Model)
import Model.Balance exposing (Account, BalanceRef(..), Bucket)
import Page.Overview.Model exposing (..)
import View.RightClick exposing (onRightClick)


page : Model -> PageState -> Html Msg
page model state =
    let
        accounts =
            List.map (renderAccount state.editing) model.balances.accounts

        buckets =
            List.map (renderBucket state.editing) model.balances.buckets
    in
        div [ class "container_24" ]
            [ div [ class "grid_8" ] (accounts ++ [ renderNewAccount ])
            , div [ class "prefix_4 grid_12" ] (buckets ++ [ renderNewBucket ])
            ]


renderAccount : Maybe BalanceRef -> Account -> Html Msg
renderAccount editingBalanceRef account =
    div [ class "alpha grid_8 omega" ]
        [ renderName (AccountRef account.id) account.name editingBalanceRef
        , div [ class "grid_4 currency omega" ] [ text <| toString account.amount ]
        ]


renderBucket : Maybe BalanceRef -> Bucket -> Html Msg
renderBucket editingBalanceRef bucket =
    div [ class "alpha grid_12 omega" ]
        [ renderName (BucketRef bucket.id) bucket.name editingBalanceRef
        , div [ class "grid_4 currency" ] [ text <| toString bucket.rate ]
        , div [ class "grid_4 currency omega" ] [ text <| toString bucket.amount ]
        ]


renderName : BalanceRef -> String -> Maybe BalanceRef -> Html Msg
renderName balanceRef name editingBalanceRef =
    case editingBalanceRef of
        Just bId ->
            if bId == balanceRef then
                input [ class "alpha grid_4 niceInput", type_ "text", value name ] []
            else
                simpleRenderName balanceRef name

        _ ->
            simpleRenderName balanceRef name


simpleRenderName : BalanceRef -> String -> Html Msg
simpleRenderName balanceRef name =
    div [ class "alpha grid_4", onClick (OpenTransactionsBalance balanceRef), onRightClick (StartEditingBalanceName balanceRef) ] [ text name ]


renderNewAccount : Html Msg
renderNewAccount =
    div [ class "alpha grid_8 omega newStuff", onClick NewAccount ] [ text "New Account" ]


renderNewBucket : Html Msg
renderNewBucket =
    div [ class "alpha grid_12 omega newStuff", onClick NewBucket ] [ text "New Bucket" ]
