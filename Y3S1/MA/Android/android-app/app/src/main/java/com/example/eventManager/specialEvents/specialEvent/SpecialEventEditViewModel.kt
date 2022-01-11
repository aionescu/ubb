package com.example.eventManager.specialEvents.specialEvent

import android.app.Application
import android.util.Log
import androidx.lifecycle.*
import androidx.work.*
import com.example.eventManager.core.TAG
import com.example.eventManager.specialEvents.data.SpecialEvent
import com.example.eventManager.specialEvents.data.SpecialEventsRepository
import kotlinx.coroutines.launch
import com.example.eventManager.core.Result
import com.example.eventManager.specialEvents.data.SyncWorker
import com.example.eventManager.specialEvents.data.local.SpecialEventsDatabase
import java.util.*

class SpecialEventEditViewModel(application: Application) : AndroidViewModel(application) {
    private val mutableSpecialEvent = MutableLiveData<SpecialEvent>().apply { value = SpecialEvent("", "", 0, Date(),false) }

    private val mutableFetching = MutableLiveData<Boolean>().apply { value = false }
    private val mutableCompleted = MutableLiveData<Boolean>().apply { value = false }
    private val mutableException = MutableLiveData<Exception>().apply { value = null }

    val specialEvent: LiveData<SpecialEvent> = mutableSpecialEvent

    val fetching: LiveData<Boolean> = mutableFetching
    val fetchingError: LiveData<Exception> = mutableException
    val completed: LiveData<Boolean> = mutableCompleted

    val specialEventsRepository: SpecialEventsRepository
    init {
        val specialEventDao = SpecialEventsDatabase.getDatabase(application, viewModelScope).specialEventDao()
        specialEventsRepository = SpecialEventsRepository(specialEventDao)
    }

    private val TAG_OUTPUT = "OUTPUT"

    fun getSpecialEventById(specialEventId: String): LiveData<SpecialEvent> {
        Log.v(TAG, "getSpecialEventById...")
        return specialEventsRepository.getById(specialEventId)
    }

    fun saveOrUpdateSpecialEvent(specialEvent: SpecialEvent) {
        viewModelScope.launch {
            viewModelScope.launch {
                Log.v(TAG, "saveOrUpdateSpecialEvent...");

                mutableFetching.value = true
                mutableException.value = null

                val result: Result<SpecialEvent>
                var operation = ""

                if (specialEvent._id.isNotEmpty()) {
                    operation = "update"
                    result = specialEventsRepository.update(specialEvent)
                } else {
                    var id = generateRandomString(10)
                    specialEvent._id = id
                    operation = "save"
                    result = specialEventsRepository.save(specialEvent)
                }
                when (result) {
                    is Result.Success -> {
                        Log.d(TAG, "saveOrUpdateSpecialEvent succeeded");
                    }
                    is Result.Error -> {
                        Log.w(TAG, "saveOrUpdateSpecialEvent failed", result.exception);
                        mutableException.value = result.exception
                    }
                }
                mutableCompleted.value = true
                mutableFetching.value = false
            }
        }
    }

    fun generateRandomString(length: Int) : String {
        val allowedChars = ('A'..'Z') + ('a'..'z') + ('0'..'9')
        return (1..length)
            .map { allowedChars.random() }
            .joinToString("")
    }

    private fun createJob(operation: String, specialEvent: SpecialEvent): OneTimeWorkRequest {
        val constraints = Constraints.Builder()
            .setRequiredNetworkType(NetworkType.CONNECTED)
            .build()

        val inputData = Data.Builder()
            .putString("operation", operation)
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
            .addTag(TAG_OUTPUT)
            .build()

        return myWork
    }
}