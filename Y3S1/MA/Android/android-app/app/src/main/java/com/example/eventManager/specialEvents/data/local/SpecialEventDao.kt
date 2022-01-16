package com.example.eventManager.specialEvents.data.local

import androidx.lifecycle.LiveData
import androidx.room.*
import com.example.eventManager.specialEvents.data.SpecialEvent


@Dao
interface SpecialEventDao {
    @Query("SELECT * from specialEvents ORDER BY title ASC")
    fun getAll(): LiveData<List<SpecialEvent>>

    @Query("SELECT * FROM specialEvents WHERE _id=:id ")
    fun getById(id: String): LiveData<SpecialEvent>

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    suspend fun insert(specialEvent: SpecialEvent)

    @Update(onConflict = OnConflictStrategy.REPLACE)
    suspend fun update(specialEvents: SpecialEvent)

    @Query("DELETE FROM specialEvents")
    suspend fun deleteAll()

}