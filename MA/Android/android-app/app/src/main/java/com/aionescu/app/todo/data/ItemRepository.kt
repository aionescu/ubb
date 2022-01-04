package com.aionescu.app.todo.data

import android.util.Log
import com.aionescu.app.core.TAG
import com.aionescu.app.core.Result
import com.aionescu.app.todo.data.remote.ItemApi

object ItemRepository {
    private var cachedItems: MutableList<Item>? = null;

    suspend fun loadAll(): Result<List<Item>> {
        if (cachedItems != null) {
            Log.v(TAG, "loadAll - return cached items")
            return Result.Success(cachedItems as List<Item>);
        }
        try {
            Log.v(TAG, "loadAll - started")
            val items = ItemApi.service.find()
            Log.v(TAG, "loadAll - succeeded")
            cachedItems = mutableListOf()
            cachedItems?.addAll(items)
            return Result.Success(cachedItems as List<Item>)
        } catch (e: Exception) {
            Log.w(TAG, "loadAll - failed", e)
            return Result.Error(e)
        }
    }

    suspend fun load(itemId: String): Result<Item> {
        val item = cachedItems?.find { it._id == itemId }
        if (item != null) {
            Log.v(TAG, "load - return cached item")
            return Result.Success(item)
        }
        try {
            Log.v(TAG, "load - started")
            val itemRead = ItemApi.service.read(itemId)
            Log.v(TAG, "load - succeeded")
            return Result.Success(itemRead)
        } catch (e: Exception) {
            Log.w(TAG, "load - failed", e)
            return Result.Error(e)
        }
    }

    suspend fun save(item: Item): Result<Item> {
        try {
            Log.v(TAG, "save - started")
            val createdItem = ItemApi.service.create(item)
            Log.v(TAG, "save - succeeded")
            cachedItems?.add(createdItem)
            return Result.Success(createdItem)
        } catch (e: Exception) {
            Log.w(TAG, "save - failed", e)
            return Result.Error(e)
        }
    }

    suspend fun update(item: Item): Result<Item> {
        try {
            Log.v(TAG, "update - started")
            val updatedItem = ItemApi.service.update(item._id, item)
            val index = cachedItems?.indexOfFirst { it._id == item._id }
            if (index != null) {
                cachedItems?.set(index, updatedItem)
            }
            Log.v(TAG, "update - succeeded")
            return Result.Success(updatedItem)
        } catch (e: Exception) {
            Log.v(TAG, "update - failed")
            return Result.Error(e)
        }
    }
}