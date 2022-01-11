package com.example.eventManager.specialEvents.specialEvents

import android.app.Application
import android.util.Log
import androidx.lifecycle.*
import com.example.eventManager.core.TAG
import com.example.eventManager.specialEvents.data.SpecialEvent
import com.example.eventManager.specialEvents.data.SpecialEventsRepository
import kotlinx.coroutines.launch
import com.example.eventManager.core.Result
import com.example.eventManager.specialEvents.data.local.SpecialEventsDatabase

class SpecialEventListViewModel(application: Application) : AndroidViewModel(application) {
    private val mutableLoading = MutableLiveData<Boolean>().apply { value = false }
    private val mutableException = MutableLiveData<Exception>().apply { value = null }

    val specialEvents: LiveData<List<SpecialEvent>>
    val loading: LiveData<Boolean> = mutableLoading
    val loadingError: LiveData<Exception> = mutableException

    val specialEventsRepository: SpecialEventsRepository

    init {
        val specialEventDao = SpecialEventsDatabase.getDatabase(application, viewModelScope).specialEventDao()
        specialEventsRepository = SpecialEventsRepository(specialEventDao)
        specialEvents = specialEventsRepository.specialEvents
    }

    fun refresh() {
        viewModelScope.launch {
            Log.v(TAG, "loadSpecialEvents...");
            mutableLoading.value = true
            mutableException.value = null
            when (val result = specialEventsRepository.refresh()) {
                is Result.Success -> {
                    Log.d(TAG, "refresh succeeded");
                }
                is Result.Error -> {
                    Log.w(TAG, "refresh failed", result.exception);
                    mutableException.value = result.exception
                }
            }
            mutableLoading.value = false
        }
    }
}