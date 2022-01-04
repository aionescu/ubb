package com.aionescu.app.todo.data

data class Item(
    val _id: String,
    var text: String
) {
    override fun toString(): String = "{ \"_id\": \"$_id\", \"text\": \"$text\" }"
}
