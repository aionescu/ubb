package com.example.eventManager.specialEvents.data

import androidx.room.ColumnInfo
import androidx.room.Entity
import androidx.room.PrimaryKey
import java.util.*

@Entity(tableName = "specialEvents")
data class SpecialEvent(
    @PrimaryKey @ColumnInfo(name = "_id") var _id: String,
    @ColumnInfo(name = "title") var title: String,
    @ColumnInfo(name = "numberOfPeople") var numberOfPeople: Int,
    @ColumnInfo(name = "date") var date: Date,
    @ColumnInfo(name = "isApproved") var isApproved: Boolean
) {
    override fun toString(): String = title
}