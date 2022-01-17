package ro.arc.packageManager.client.networking;

import java.io.IOException;
import java.net.Socket;

import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.networking.Message;

public class TCPClient {
  public static Message sendAndReceive(Message request) {
    try (
      var socket = new Socket("localhost", 1234);
      var inputStream = socket.getInputStream();
      var outputStream = socket.getOutputStream()
    ) {
      Message.write(request, outputStream);
      return Message.read(inputStream);
    } catch (IOException e) {
      throw new AppException("Connection to server failed: " + e.getMessage());
    }
  }
}
