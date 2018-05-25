module Page.Overview.Page exposing (page)

{-
   Copyright 2018 Daan Wendelen

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

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick)
import Model.Application exposing (Model)
import Model.Balance exposing (Account, BalanceRef(..), Bucket, getBufferLostValue, leakedInBucket)
import Model.Limbo exposing (getAccountLimbo, getBucketLimbo)
import Model.Transaction exposing (getAmount)
import Page.Overview.Model exposing (..)
import View.RightClick exposing (onRightClick)


page : Model -> PageState -> Html Msg
page model state =
    let
        accountLimbo =
            renderAccountLimbo <| getAccountLimbo model.transactions

        bucketLimbo =
            renderBucketLimbo <| getBucketLimbo model.transactions

        accounts =
            List.map (renderAccount state.editing model) model.balances.accounts

        buffer =
            renderBuffer model

        buckets =
            List.map (renderBucket state.editing model) model.balances.buckets
    in
        div [ class "container_24" ]
            [ div [ class "grid_8" ]
                ([ accountLimbo ] ++ accounts ++ [ renderNewAccount ])
            , div [ class "prefix_4 grid_12" ]
                ([ bucketLimbo, buffer ] ++ buckets ++ [ renderNewBucket ])
            ]


renderAccount : Maybe BalanceRef -> Model -> Account -> Html Msg
renderAccount editingBalanceRef model account =
    div [ class "alpha grid_8 omega" ]
        [ renderName (AccountRef account.id) account.name editingBalanceRef
        , div [ class "grid_4 currency omega" ] [ text <| toString (getAmount (AccountRef account.id) model.transactions) ]
        ]


renderBucket : Maybe BalanceRef -> Model -> Bucket -> Html Msg
renderBucket editingBalanceRef model bucket =
    div [ class "alpha grid_12 omega" ]
        [ renderName (BucketRef bucket.id) bucket.name editingBalanceRef
        , div [ class "grid_4 currency" ] [ text <| toString <| getBucketAmount bucket model ]
        , div [ class "grid_4 currency omega" ] [ text <| toString bucket.rate ]
        ]

getBucketAmount bucket model =
    let
        fromTransactions =
            -1 * (getAmount (BucketRef bucket.id) model.transactions)
        fromLeaking =
            leakedInBucket model.currentTime bucket
    in
        fromTransactions + fromLeaking

renderName : BalanceRef -> String -> Maybe BalanceRef -> Html Msg
renderName balanceRef name editingBalanceRef =
    case editingBalanceRef of
        Just bId ->
            if bId == balanceRef then
                input [ id (renderNameId balanceRef), class "alpha grid_4 niceInput", type_ "text", value name ] []
            else
                simpleRenderName balanceRef name

        _ ->
            simpleRenderName balanceRef name


renderNameId : BalanceRef -> String
renderNameId balanceRef =
    case balanceRef of
        BufferRef ->
            "buffer"

        NoBalanceRef ->
            "null"

        AccountRef aId ->
            "a" ++ toString aId ++ "_name"

        BucketRef bId ->
            "b" ++ toString bId ++ "_name"


simpleRenderName : BalanceRef -> String -> Html Msg
simpleRenderName balanceRef name =
    div [ class "alpha grid_4", onClick (OpenTransactionsBalance balanceRef), onRightClick (StartEditingBalanceName balanceRef) ] [ text name ]


renderNewAccount : Html Msg
renderNewAccount =
    div [ class "alpha grid_8 omega newStuff", onClick NewAccount ] [ text "New Account" ]


renderNewBucket : Html Msg
renderNewBucket =
    div [ class "alpha grid_12 omega newStuff", onClick NewBucket ] [ text "New Bucket" ]


renderAccountLimbo : Float -> Html Msg
renderAccountLimbo accountLimbo =
    div [ class "alpha grid_8 omega" ]
        [ div [ class "alpha grid_4" ] [ text "Limbo" ]
        , div [ class "grid_4 currency omega" ] [ text <| toString accountLimbo ]
        ]


renderBucketLimbo : Float -> Html Msg
renderBucketLimbo bucketLimbo =
    div [ class "alpha grid_12 omega" ]
        [ div [ class "alpha grid_4" ] [ text "Limbo" ]
        , div [ class "grid_4 suffix_4 omega currency" ] [ text <| toString <| -1 * bucketLimbo ]
        ]


renderBuffer : Model -> Html Msg
renderBuffer model =
    div [ class "alpha grid_12 omega" ]
        [ div [ class "alpha grid_4", onClick (OpenTransactionsBalance BufferRef) ] [ text "Buffer" ]
        , div [ class "grid_4 currency suffix_4 omega" ] [ text <| toString <| getBufferAmount model ]
        ]

getBufferAmount : Model -> Float
getBufferAmount model =
    let
        fromTransactions =
            -1 * (getAmount BufferRef model.transactions)
        fromLeakage =
             Debug.log "leak" <| getBufferLostValue model.balances model.currentTime
    in
        fromTransactions + fromLeakage