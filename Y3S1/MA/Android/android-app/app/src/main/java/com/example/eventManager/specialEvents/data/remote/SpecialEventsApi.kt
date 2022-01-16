package com.example.eventManager.specialEvents.data.remote

import com.example.eventManager.core.Api
import com.example.eventManager.specialEvents.data.SpecialEvent
import retrofit2.http.*

object SpecialEventsApi {
    interface Service {
        @GET("api/specialEvent")
        suspend fun find(): List<SpecialEvent>

        @GET("/api/specialEvent/{id}")
        suspend fun read(@Path("id") specialEventId: String): SpecialEvent;

        @Headers("Content-Type: application/json")
        @POST("/api/specialEvent")
        suspend fun create(@Body specialEvent: SpecialEvent): SpecialEvent

        @Headers("Content-Type: application/json")
        @PUT("/api/specialEvent/{id}")
        suspend fun update(@Path("id") specialEventId: String, @Body specialEvent: SpecialEvent): SpecialEvent
    }

    val service: Service = Api.retrofit.create(Service::class.java)
}