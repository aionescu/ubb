package ro.arc.packageManager.common.networking;

import java.io.*;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Message {
  private final String header;
  private List<String> body;
  public static String MessageError = "error";
  public static String MessageSuccess = "success";

  public Message(String header, Object... body) {
    this(header, Arrays.stream(body));
  }

  public Message(String header, Stream<Object> body) {
    this.header = header;
    this.body = body.map(Object::toString).collect(Collectors.toList());
  }

  public static Message read(InputStream inputStream) throws IOException {
    DataInputStream dataInputStream = new DataInputStream(inputStream);
    String header = dataInputStream.readUTF();
    Message message = new Message(header);
    int bodySize = dataInputStream.readInt();
    IntStream.range(0, bodySize).forEach(row -> {
      try {
        message.addRow(dataInputStream.readUTF());
      } catch (IOException e) {
        e.printStackTrace();
      }
    });
    return message;
  }

  public static void write(Message message, OutputStream stream) throws IOException {
    DataOutputStream dataOutputStream = new DataOutputStream(stream);
    dataOutputStream.writeUTF(message.header);
    dataOutputStream.writeInt(message.body.size());

    message.body.forEach(row -> {
      try {
        dataOutputStream.writeUTF(row);
      } catch (IOException e) {
        e.printStackTrace();
      }
    });
  }

  public void addRow(String string) {
    body.add(string);
  }

  public String getHeader() {
    return header;
  }

  public List<String> getBody() {
    return body;
  }

  public void setBody(List<String> body) {
    this.body = body;
  }

}
