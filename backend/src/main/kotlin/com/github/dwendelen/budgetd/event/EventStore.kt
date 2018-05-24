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

package com.github.dwendelen.budgetd.event

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.dwendelen.eventstored.client.Driver
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono

class EventStore(val driver: Driver, val objectMapper: ObjectMapper) {

    fun getEvents(start: Long): Flux<Pair<Long, Event>> {
        val publisher = driver.streamEvents(ByteArray(0), start, Long.MAX_VALUE, true)
        return Flux.from(publisher)
                .map {
                    val event = objectMapper.readValue(it.data.toByteArray(), Event::class.java)
                    it.index to event
                }
    }

    fun storeEvent(event: Event): Mono<Long> {
        val data = objectMapper.writeValueAsBytes(event)
        val newId = driver.addEvent(data)

        return Mono.from(newId)
    }
}