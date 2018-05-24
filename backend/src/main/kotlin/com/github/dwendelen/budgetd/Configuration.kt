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

package com.github.dwendelen.budgetd

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.dwendelen.budgetd.event.EventStore
import com.github.dwendelen.budgetd.socket.SocketHandler
import com.github.dwendelen.eventstored.client.Driver
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.web.reactive.HandlerMapping
import org.springframework.web.reactive.handler.SimpleUrlHandlerMapping
import org.springframework.web.reactive.socket.WebSocketHandler
import org.springframework.web.reactive.socket.server.support.WebSocketHandlerAdapter


@Configuration
class WebSocketConfiguration {

    @Bean
    fun driver(): Driver {
        val driver = Driver("localhost", 7070)
        driver.start()
        return driver
    }

    @Bean
    fun eventStore(objectMapper: ObjectMapper): EventStore {
        return EventStore(driver(), objectMapper)
    }

    @Bean
    fun webSocketHandlerAdapter(): WebSocketHandlerAdapter {
        return WebSocketHandlerAdapter()
    }

    @Bean
    fun handlerMapping(eventWebsocketHandler: WebSocketHandler): HandlerMapping {
        val mapping = SimpleUrlHandlerMapping()
        mapping.order = 10
        mapping.urlMap = mapOf("/events" to eventWebsocketHandler)

        return mapping
    }

    @Bean
    fun eventWebsocketHandler(eventStore: EventStore, objectMapper: ObjectMapper): WebSocketHandler {
        return SocketHandler(eventStore, objectMapper)
    }
}