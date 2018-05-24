{-
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


module Model.Socket
    exposing
        ( initialMessage
        , initialSocket
        , subscriptions
        , handle
        , Msg
        , Event(..)
        , Socket
        )

import Json.Decode exposing (Decoder, decodeString, field, float, int, string)
import Model.Balance exposing (BalanceRef)
import Model.Transaction exposing (Amount, Comment, Date, SubTransactionId)
import View.BalancesDropDown exposing (stringToBalanceRef)
import WebSocket


type alias Socket =
    { status : Status
    }


initialSocket : Socket
initialSocket =
    { status = Open { lastId = -1 } }


type Status
    = Closed
    | Open SocketInfo


type alias SocketInfo =
    { lastId : Int
    }


type Msg
    = IncomingSocketMessage String


type SocketMessage
    = EventStored Int
    | EventHappened EventHappenedData


type alias EventHappenedData =
    { idx : Int, event : Event }


type Event
    = CreateSubTransactionEvent CreateEventData
    | UpdateDateEvent DateEventData
    | UpdateCommentEvent CommentEventData
    | UpdateBalanceEvent BalanceEventData
    | UpdateAmountEvent AmountEventData
    | DeleteSubTransactionEvent SubTransactionId
    | CreateBucketEvent CreateBucketEventData
    | CreateAccountEvent CreateAccountEventData


type alias CreateEventData =
    { subTransactionId : Int
    , transactionId : Int
    , date : Date
    , balance : BalanceRef
    , comment : Comment
    , amount : Amount
    }


type alias DateEventData =
    { subTransactionId : Int
    , date : Date
    }


type alias CommentEventData =
    { subTransactionId : Int
    , comment : String
    }


type alias BalanceEventData =
    { subTransactionId : Int
    , balance : BalanceRef
    }


type alias AmountEventData =
    { subTransactionId : Int
    , amount : Amount
    }


type alias CreateBucketEventData =
    { bucketId : Int
    , name : String
    }


type alias CreateAccountEventData =
    { accountId : Int
    , name : String
    }


url =
    "ws://localhost:8070/events"


initialMessage : Cmd msg
initialMessage =
    WebSocket.send url """{"type": "START_SENDING","startIdx": 0}"""


subscriptions : Sub Msg
subscriptions =
    WebSocket.listen url IncomingSocketMessage


handle : Msg -> Socket -> ( Socket, Maybe Event )
handle incoming socket =
    case socket.status of
        Closed ->
            ( socket, Nothing )

        Open socketInfo ->
            case incoming of
                IncomingSocketMessage json ->
                    let
                        maybeSocketMessage =
                            Debug.log "maybeSocMsg" <| parseSocketMessage json
                    in
                        handleSocketMessage maybeSocketMessage socket socketInfo


handleSocketMessage : Maybe SocketMessage -> Socket -> SocketInfo -> ( Socket, Maybe Event )
handleSocketMessage maybeSocketMessage socket socketInfo =
    case maybeSocketMessage of
        Nothing ->
            ( socket, Nothing )

        Just (EventStored _) ->
            ( socket, Nothing )

        Just (EventHappened data) ->
            if data.idx > socketInfo.lastId then
                let
                    newSocketInfo =
                        { socketInfo | lastId = data.idx }

                    newSocket =
                        { socket | status = Open newSocketInfo }
                in
                    ( newSocket, Just data.event )
            else
                ( socket, Nothing )


parseSocketMessage : String -> Maybe SocketMessage
parseSocketMessage json =
    case Debug.log "parseType" <| parseType json of
        Just "EVENT_HAPPENED" ->
            parseEventHappened json

        Just "EVENT_STORED" ->
            parseEventStored json

        _ ->
            Nothing


parseType : String -> Maybe String
parseType json =
    decodeString typeDecoder json
        |> Result.toMaybe


typeDecoder : Decoder String
typeDecoder =
    field "type" string


parseEventStored : String -> Maybe SocketMessage
parseEventStored json =
    decodeString eventStoredDecoder json
        |> Result.toMaybe


eventStoredDecoder : Decoder SocketMessage
eventStoredDecoder =
    Json.Decode.map EventStored (field "idx" int)


parseEventHappened : String -> Maybe SocketMessage
parseEventHappened json =
    decodeString eventHappenedDataDecoder json
        |> Result.mapError (Debug.log "parseErr")
        |> Result.toMaybe


eventHappenedDataDecoder : Decoder SocketMessage
eventHappenedDataDecoder =
    Json.Decode.map2
        EventHappenedData
        (field "idx" int)
        (field "event" eventDecoder)
        |> Json.Decode.map EventHappened


eventDecoder : Decoder Event
eventDecoder =
    Json.Decode.oneOf
        [ createSubTransactionDecoder
        , updateDateDecoder
        , updateBalanceDecoder
        , updateCommentDecoder
        , updateAmountDecoder
        , createAccountDecoder
        , createBucketDecoder
        , deleteSubTransactionDecoder
        ]


createSubTransactionDecoder : Decoder Event
createSubTransactionDecoder =
    Json.Decode.map6
        CreateEventData
        (field "subTransactionId" int)
        (field "transactionId" int)
        (field "date" string)
        (field "balance" balanceRefDecoder)
        (field "comment" string)
        (field "amount" float)
        |> Json.Decode.map CreateSubTransactionEvent


updateDateDecoder : Decoder Event
updateDateDecoder =
    Json.Decode.map2
        DateEventData
        (field "subTransactionId" int)
        (field "date" string)
        |> Json.Decode.map UpdateDateEvent


updateCommentDecoder : Decoder Event
updateCommentDecoder =
    Json.Decode.map2
        CommentEventData
        (field "subTransactionId" int)
        (field "comment" string)
        |> Json.Decode.map UpdateCommentEvent


updateBalanceDecoder : Decoder Event
updateBalanceDecoder =
    Json.Decode.map2
        BalanceEventData
        (field "subTransactionId" int)
        (field "balance" balanceRefDecoder)
        |> Json.Decode.map UpdateBalanceEvent


updateAmountDecoder : Decoder Event
updateAmountDecoder =
    Json.Decode.map2
        AmountEventData
        (field "subTransactionId" int)
        (field "amount" float)
        |> Json.Decode.map UpdateAmountEvent


deleteSubTransactionDecoder : Decoder Event
deleteSubTransactionDecoder =
    Json.Decode.map
        DeleteSubTransactionEvent
        (field "subTransactionId" int)


createBucketDecoder : Decoder Event
createBucketDecoder =
    Json.Decode.map2
        CreateBucketEventData
        (field "bucketId" int)
        (field "name" string)
        |> Json.Decode.map CreateBucketEvent


createAccountDecoder : Decoder Event
createAccountDecoder =
    Json.Decode.map2
        CreateAccountEventData
        (field "accountId" int)
        (field "name" string)
        |> Json.Decode.map CreateAccountEvent


balanceRefDecoder : Decoder BalanceRef
balanceRefDecoder =
    Json.Decode.map
        stringToBalanceRef
        string
