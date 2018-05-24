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

package com.github.dwendelen.budgetd.event;

import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import java.time.LocalDateTime

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes(
        JsonSubTypes.Type(value = CreateSubTransaction::class, name = "CREATE_SUB_TRANSACTION"),
        JsonSubTypes.Type(value = UpdateDate::class, name="UPDATE_DATE"),
        JsonSubTypes.Type(value = UpdateComment::class, name="UPDATE_COMMENT"),
        JsonSubTypes.Type(value = UpdateBalance::class, name="UPDATE_BALANCE"),
        JsonSubTypes.Type(value = UpdateAmount::class, name="UPDATE_AMOUNT"),
        JsonSubTypes.Type(value = DeleteSubTransaction::class, name="DELETE_SUB_TRANSACTION"),
        JsonSubTypes.Type(value = CreateBucket::class, name="CREATE_BUCKET"),
        JsonSubTypes.Type(value = CreateAccount::class, name="CREATE_ACCOUNT")

)
interface Event

data class CreateSubTransaction(
        val subTransactionId: Int,
        val transactionId: Int,
        val date: String,
        val balance: String,
        val comment: String,
        val amount: Double
): Event

data class UpdateDate(
        val subTransactionId: Int,
        val date: String
): Event

data class UpdateComment(
        val subTransactionId: Int,
        val comment: String
): Event

data class UpdateBalance(
        val subTransactionId: Int,
        val balance: String
): Event

data class UpdateAmount(
        val subTransactionId: Int,
        val amount: Double
): Event

data class DeleteSubTransaction(
        val subTransactionId: Int
): Event

data class CreateBucket(
        val bucketId: Int,
        val name: String
): Event

data class CreateAccount(
        val accountId: Int,
        val name: String
): Event
