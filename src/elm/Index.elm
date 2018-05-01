module Main exposing (..)

import Html exposing (Html, datalist, div, input, node, option, program, select, text)
import Model.Application exposing (..)
import Page.Overview.Model
import Page.Overview.Page
import Page.Overview.Update
import Page.Transactions.Model
import Page.Transactions.Page
import Page.Transactions.Update


main : Program Never Model Msg
main =
    program
        { init = ( Model.Application.initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = TransactionMsg Page.Transactions.Model.Msg
    | OverviewPageMsg Page.Overview.Model.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TransactionMsg transMsg ->
            case model.page of
                TransactionsBalance _ ->
                    ( Page.Transactions.Update.update transMsg model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OverviewPageMsg overviewMsg ->
            case model.page of
                Overview pageModel ->
                    ( Page.Overview.Update.update pageModel model overviewMsg, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
