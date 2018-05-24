/*
   Copyright 2018 Cegeka

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

package com.github.dwendelen.budgetd.socket

import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.github.dwendelen.budgetd.event.Event

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes(
    JsonSubTypes.Type(value = StartSending::class, name = "START_SENDING"),
    JsonSubTypes.Type(value = EventHappened::class, name="EVENT_HAPPENED"),
    JsonSubTypes.Type(value = StoreEvent::class, name="STORE_EVENT"),
    JsonSubTypes.Type(value = EventStored::class, name="EVENT_STORED")
)
interface SocketMessage

data class StartSending (val startIdx: Long) : SocketMessage
data class StoreEvent (val event: Event) : SocketMessage
data class EventStored (val idx: Long): SocketMessage
data class EventHappened (val idx: Long, val event: Event) : SocketMessage
