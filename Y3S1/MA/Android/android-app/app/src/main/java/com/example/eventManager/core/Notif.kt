package com.example.eventManager.core

import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import androidx.fragment.app.Fragment
import com.example.eventManager.MainActivity
import com.example.eventManager.R

fun createNotificationChannel(f: Fragment) {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        val name = "My channel name"
        val descriptionText = "My channel description"
        val importance = NotificationManager.IMPORTANCE_DEFAULT
        val channel = NotificationChannel("CHANNEL_ID", name, importance).apply {
            description = descriptionText
        }
        val notificationManager: NotificationManager =
            f.activity?.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        notificationManager.createNotificationChannel(channel)
    }
}

fun createNotification(f: Fragment, title: String, text: String) {
    createNotificationChannel(f)

    val intent = Intent(f.context, MainActivity::class.java).apply {
        flags = Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK
    }
    val pendingIntent: PendingIntent = PendingIntent.getActivity(f.activity, 0, intent, 0)
    val builder = f.context?.let {
        NotificationCompat.Builder(it, "CHANNEL_ID")
            .setSmallIcon(R.drawable.ic_launcher_foreground)
            .setContentTitle(title)
            .setContentText(text)
            .setPriority(NotificationCompat.PRIORITY_DEFAULT)
            //.setContentIntent(pendingIntent)
            .setAutoCancel(true)
    }
    with(NotificationManagerCompat.from(f.requireContext())) {
        if (builder != null) {
            notify(1, builder.build())
        }
    }
}
