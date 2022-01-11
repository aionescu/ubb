package com.example.eventManager.specialEvents.data

import android.util.Log
import androidx.lifecycle.LiveData
import androidx.work.*
import com.example.eventManager.core.TAG
import com.example.eventManager.core.Result
import com.example.eventManager.specialEvents.data.local.SpecialEventDao
import com.example.eventManager.specialEvents.data.remote.SpecialEventsApi

class SpecialEventsRepository(private val specialEventDao: SpecialEventDao) {

    val specialEvents = specialEventDao.getAll();

    suspend fun refresh(): Result<Boolean> {
        try {
            val specialEvents = SpecialEventsApi.service.find()
            for (specialEvent in specialEvents) {
                specialEventDao.insert(specialEvent)
            }
            return Result.Success(true)
        } catch (e: java.lang.Exception) {
            return Result.Error(e)
        }
    }


    fun getById(specialEventId: String): LiveData<SpecialEvent> {
        return specialEventDao.getById(specialEventId)
    }


    suspend fun save(specialEvent: SpecialEvent): Result<SpecialEvent> {
        try {
            Log.v(TAG, "save - started")
            val createdSpecialEvent = SpecialEventsApi.service.create(specialEvent)
            specialEventDao.insert(createdSpecialEvent)
            Log.v(TAG, "save - succeeded")
            return Result.Success(createdSpecialEvent)
        } catch (e: Exception) {
            Log.w(TAG, "save - failed", e)

            specialEventDao.insert(specialEvent)

            createWorker(specialEvent, "save")
            return Result.Error(e)
        }
    }

    suspend fun update(specialEvent: SpecialEvent): Result<SpecialEvent> {
        try {
            Log.v(TAG, "update - started")
            val updatedSpecialEvent =
                SpecialEventsApi.service.update(specialEvent._id, specialEvent)
            specialEventDao.update(updatedSpecialEvent)
            Log.v(TAG, "update - succeeded")
            return Result.Success(updatedSpecialEvent)
        } catch (e: Exception) {
            Log.v(TAG, "update - failed")
            specialEventDao.update(specialEvent)
            createWorker(specialEvent, "update")
            return Result.Error(e)
        }
    }

    fun createWorker(specialEvent: SpecialEvent, operation: String) {
        val constraints = Constraints.Builder()
            .setRequiredNetworkType(NetworkType.CONNECTED)
            .build()

        val inputData = Data.Builder()
            .putString("operation", "save")
            .putString("id", specialEvent._id)
            .putString("title", specialEvent.title)
            .putInt("numberOfPeople", specialEvent.numberOfPeople)
            .putBoolean("isApproved", specialEvent.isApproved)
            .putInt("dateDay", specialEvent.date.day)
            .putInt("dateMonth", specialEvent.date.month)
            .putInt("dateYear", specialEvent.date.year)
            .build()

        val myWork = OneTimeWorkRequest.Builder(SyncWorker::class.java)
            .setConstraints(constraints)
            .setInputData(inputData)
            .build()

        WorkManager.getInstance().enqueue(myWork);
    }

}