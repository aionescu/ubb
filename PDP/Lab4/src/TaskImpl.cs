using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using lab4.domain;
using lab4.utils;

namespace lab4.impl {
  class TaskImpl {
    private static List<string> hosts;

    public static void run(List<string> hostnames, bool async) {
      hosts = hostnames;
      var tasks = new List<Task>();

      for (var i = 0; i < hostnames.Count; i++) {
        if (async) {
          tasks.Add(Task.Factory.StartNew(DoStartAsync, i));
        } else {
          tasks.Add(Task.Factory.StartNew(DoStart, i));
        }
      }

      Task.WaitAll(tasks.ToArray());
    }

    private static void DoStartAsync(object idObject) {
      var id = (int)idObject;

      StartAsyncClient(hosts[id], id);
    }

    private static void DoStart(object idObject) {
      var id = (int)idObject;

      StartClient(hosts[id], id);
    }

    private static void StartClient(string host, int id) {
      var ipHostInfo = Dns.GetHostEntry(host.Split('/')[0]);
      var ipAddr = ipHostInfo.AddressList[0];
      var remEndPoint = new IPEndPoint(ipAddr, Parser.PORT);

      var client = new Socket(ipAddr.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

      var requestSocket = new CustomSocket {
        sock = client,
        hostname = host.Split('/')[0],
        endpoint = host.Contains("/") ? host.Substring(host.IndexOf("/", StringComparison.Ordinal)) : "/",
        remoteEndPoint = remEndPoint,
        id = id
      };

      Connect(requestSocket).Wait(); // connect to remote server
      Send(requestSocket, Parser.GetRequestString(requestSocket.hostname, requestSocket.endpoint))
        .Wait(); // request data from server
      Receive(requestSocket).Wait(); // receive server response

      Console.WriteLine("Connection {0} > Content length is:{1}", requestSocket.id, Parser.GetContentLen(requestSocket.responseContent.ToString()));

      // release the socket
      client.Shutdown(SocketShutdown.Both);
      client.Close();
    }

    private static async void StartAsyncClient(string host, int id) {
      var ipHostInfo = Dns.GetHostEntry(host.Split('/')[0]);
      var ipAddress = ipHostInfo.AddressList[0];
      var remoteEndpoint = new IPEndPoint(ipAddress, Parser.PORT);

      // create the TCP/IP socket
      var client = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp); // create client socket

      var requestSocket = new CustomSocket {
        sock = client,
        hostname = host.Split('/')[0],
        endpoint = host.Contains("/") ? host.Substring(host.IndexOf("/", StringComparison.Ordinal)) : "/",
        remoteEndPoint = remoteEndpoint,
        id = id
      }; // state object

      await ConnectAsync(requestSocket); // connect to remote server

      await SendAsync(requestSocket,
        Parser.GetRequestString(requestSocket.hostname, requestSocket.endpoint)); // request data from the server

      await ReceiveAsync(requestSocket); // receive server response

      Console.WriteLine("Connection {0} > Content length is:{1}", requestSocket.id, Parser.GetContentLen(requestSocket.responseContent.ToString()));

      // release the socket
      client.Shutdown(SocketShutdown.Both);
      client.Close();
    }

    private static async Task ConnectAsync(CustomSocket state) {
      state.sock.BeginConnect(state.remoteEndPoint, ConnectCallback, state);

      await Task.FromResult<object>(state.connectDone.WaitOne()); // block until signaled
    }

    private static Task Connect(CustomSocket state) {
      state.sock.BeginConnect(state.remoteEndPoint, ConnectCallback, state);

      return Task.FromResult(state.connectDone.WaitOne()); // block until signaled
    }

    private static void ConnectCallback(IAsyncResult ar) {
      // retrieve the details from the connection information wrapper
      var resultSocket = (CustomSocket)ar.AsyncState;
      var clientSocket = resultSocket.sock;
      var clientId = resultSocket.id;
      var hostname = resultSocket.hostname;

      clientSocket.EndConnect(ar); // complete connection

      Console.WriteLine("Connection {0} > Socket connected to {1} ({2})", clientId, hostname, clientSocket.RemoteEndPoint);

      resultSocket.connectDone.Set(); // signal connection is up
    }

    private static async Task SendAsync(CustomSocket state, string data) {
      var byteData = Encoding.ASCII.GetBytes(data);

      // send data
      state.sock.BeginSend(byteData, 0, byteData.Length, 0, SendCallback, state);

      await Task.FromResult<object>(state.sendDone.WaitOne());
    }

    private static Task Send(CustomSocket state, string data) {
      // convert the string data to byte data using ASCII encoding.
      var byteData = Encoding.ASCII.GetBytes(data);

      // send data
      state.sock.BeginSend(byteData, 0, byteData.Length, 0, SendCallback, state);

      return Task.FromResult(state.sendDone.WaitOne());
    }

    private static void SendCallback(IAsyncResult ar) {
      var resultSocket = (CustomSocket)ar.AsyncState;
      var clientSocket = resultSocket.sock;
      var clientId = resultSocket.id;

      var bytesSent = clientSocket.EndSend(ar); // complete sending the data to the server

      Console.WriteLine("Connection {0} > Sent {1} bytes to server.", clientId, bytesSent);

      resultSocket.sendDone.Set(); // signal that all bytes have been sent
    }

    private static async Task ReceiveAsync(CustomSocket state) {
      // receive data
      state.sock.BeginReceive(state.buffer, 0, CustomSocket.BUFF_SIZE, 0, ReceiveCallback, state);

      await Task.FromResult<object>(state.receiveDone.WaitOne());
    }

    private static Task Receive(CustomSocket state) {
      // receive data
      state.sock.BeginReceive(state.buffer, 0, CustomSocket.BUFF_SIZE, 0, ReceiveCallback, state);

      return Task.FromResult(state.receiveDone.WaitOne());
    }

    private static void ReceiveCallback(IAsyncResult ar) {
      // retrieve the details from the connection information wrapper
      var resultSocket = (CustomSocket)ar.AsyncState;
      var clientSocket = resultSocket.sock;

      try {
        // read data from the remote device.
        var bytesRead = clientSocket.EndReceive(ar);

        // get from the buffer, a number of characters <= to the buffer size, and store it in the responseContent
        resultSocket.responseContent.Append(Encoding.ASCII.GetString(resultSocket.buffer, 0, bytesRead));

        // if the response header has not been fully obtained, get the next chunk of data
        if (!Parser.ResponseHeaderObtained(resultSocket.responseContent.ToString())) {
          clientSocket.BeginReceive(resultSocket.buffer, 0, CustomSocket.BUFF_SIZE, 0, ReceiveCallback, resultSocket);
        } else {
          resultSocket.receiveDone.Set(); // signal that all bytes have been received
        }
      } catch (Exception e) {
        Console.WriteLine(e.ToString());
      }
    }
  }
}
