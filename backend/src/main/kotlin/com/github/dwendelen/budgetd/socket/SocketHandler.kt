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

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.dwendelen.budgetd.event.EventStore
import org.springframework.web.reactive.socket.WebSocketHandler
import org.springframework.web.reactive.socket.WebSocketSession
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono

class SocketHandler(val eventStore: EventStore, val objectMapper: ObjectMapper) : WebSocketHandler {
    override fun handle(session: WebSocketSession): Mono<Void> {
        val toWrite = session.receive()
                .map { objectMapper.readValue(it.payloadAsText, SocketMessage::class.java) }
                .flatMap {
                    when (it) {
                        is StartSending -> {
                            eventStore.getEvents(it.startIdx)
                                    .map { EventHappened(it.first, it.second) }
                        }
                        is StoreEvent -> {
                            eventStore.storeEvent(it.event)
                                    .map { EventStored(it) }
                        }
                        else ->
                            Flux.empty()
                    }
                }
                .map { objectMapper.writeValueAsString(it) }
                .map { session.textMessage(it) }


        return session.send(toWrite)
    }
}