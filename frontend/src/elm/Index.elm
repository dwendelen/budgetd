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


module Main exposing (..)

import Date
import Html exposing (Html, datalist, div, input, node, option, program, select, text)
import ISO8601
import Model.Application exposing (..)
import Model.Socket
import Page.Overview.Model
import Page.Overview.Page
import Page.Overview.Update
import Page.Transactions.Model
import Page.Transactions.Page
import Page.Transactions.Update
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    program
        { init = ( Model.Application.initialModel, Model.Application.initCmd )
        , view = view
        , update = update
        , subscriptions = \_ ->
            Sub.batch
                [ Model.Application.subscriptions |> Sub.map SocketMsg
                , Time.every second NewTime
                ]
        }


type Msg
    = TransactionMsg Page.Transactions.Model.Msg
    | OverviewPageMsg Page.Overview.Model.Msg
    | SocketMsg Model.Socket.Msg
    | NewTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TransactionMsg transMsg ->
            case model.page of
                TransactionsBalance _ ->
                    Page.Transactions.Update.update transMsg model

                _ ->
                    ( model, Cmd.none )

        OverviewPageMsg overviewMsg ->
            case model.page of
                Overview pageModel ->
                    Page.Overview.Update.update pageModel model overviewMsg

                _ ->
                    ( model, Cmd.none )

        SocketMsg socketMsg ->
            let
                ( newSocket, maybeEvent ) =
                    Model.Socket.handle socketMsg model.socket

                model1 =
                    { model | socket = newSocket }
            in
                case maybeEvent of
                    Just event ->
                        ( Model.Application.handleEvent event model1, Cmd.none )

                    Nothing ->
                        ( model1, Cmd.none )
        NewTime time ->
            ({model | currentDate = timeToDateString time}, Cmd.none)

view : Model -> Html Msg
view model =
    case model.page of
        Overview state ->
            Page.Overview.Page.page model state
                |> Html.map OverviewPageMsg

        Error message ->
            text <| "Error: " ++ message

        TransactionsBalance balanceRef ->
            Page.Transactions.Page.page model balanceRef
                |> Html.map TransactionMsg

timeToDateString : Time -> String
timeToDateString time =
    ISO8601.fromTime time
    |> ISO8601.toString
    |> String.left 10