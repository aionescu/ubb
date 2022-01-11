package com.example.eventManager.specialEvents.data

import android.content.Context
import android.util.Log
import androidx.work.CoroutineWorker
import androidx.work.WorkerParameters
import com.example.eventManager.core.TAG
import com.example.eventManager.specialEvents.data.remote.SpecialEventsApi
import java.util.*

class SyncWorker (context: Context,
                  workerParams: WorkerParameters
) : CoroutineWorker(context, workerParams) {

    override suspend fun doWork(): Result {
        val operation = inputData.getString("operation")
        val id = inputData.getString("id").orEmpty()
        val title = inputData.getString("title").orEmpty()
        val numberOfPeople = inputData.getInt("numberOfPeople", 0)
        val isApproved = inputData.getBoolean("isApproved", false)
        val dateDay = inputData.getInt("dateDay", 1)
        val dateMonth = inputData.getInt("dateMonth", 1)
        val dateYear = inputData.getInt("dateYear", 2022)
        val date = Date(dateYear, dateMonth, dateDay)

        val e = SpecialEvent(id, title, numberOfPeople, date, isApproved)

        try {
            Log.v(TAG, "sync - started")
            if(operation.equals("save")){
                val createdSpecialEvent = SpecialEventsApi.service.create(e)
            }
            else if(operation.equals("update")){
                val updatedSpecialEvent = SpecialEventsApi.service.update(id, e)
            }

            Log.v(TAG, "sync - succeeded")
            return Result.success()
        } catch (e: Exception) {
            Log.w(TAG, "sync - failed", e)
            return Result.failure()
        }

    }

}
