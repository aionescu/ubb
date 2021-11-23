using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

static class CallbackImpl {
  public static void Run(List<string> hostnames) {
    CountdownEvent cde = new(hostnames.Count);

    for (var i = 0; i < hostnames.Count; ++i)
      StartClient(hostnames[i], i, cde);

    cde.Wait();
  }

  private static void StartClient(string host, int id, CountdownEvent cde) {
    var hostName = host.Split('/')[0];
    var hostEntry = Dns.GetHostEntry(hostName);
    var ipAddr = hostEntry.AddressList[0];
    var remoteEndpoint = new IPEndPoint(ipAddr, Parser.Port);

    var client = new Socket(ipAddr.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

    var requestSocket = new SocketWrapper {
      Socket = client,
      HostName = hostName,
      Endpoint = host.Contains('/') ? host[host.IndexOf('/') ..] : "/",
      IPEndpoint = remoteEndpoint,
      ID = id,
      CDE = cde
    };

    client.BeginConnect(remoteEndpoint, Connected, requestSocket);
  }

  private static void Connected(IAsyncResult result) {
    var wrapper = (SocketWrapper)result.AsyncState;
    var socket = wrapper.Socket;
    var id = wrapper.ID;
    var hostname = wrapper.HostName;

    socket.EndConnect(result);

    Console.WriteLine($"Connection {id} > Socket connected to {hostname} ({socket.RemoteEndPoint})");

    var data = Encoding.ASCII.GetBytes(Parser.RequestString(wrapper.HostName, wrapper.Endpoint));
    socket.BeginSend(data, 0, data.Length, 0, Sent, wrapper);
  }

  private static void Sent(IAsyncResult result) {
    var wrapper = (SocketWrapper)result.AsyncState;
    var socket = wrapper.Socket;
    var id = wrapper.ID;

    var sent = socket.EndSend(result);
    Console.WriteLine($"Connection {id} > Sent {sent} bytes to server.");

    socket.BeginReceive(wrapper.Buffer, 0, SocketWrapper.BufferSize, 0, Receiving, wrapper);
  }

  private static void Receiving(IAsyncResult result) {
    var wrapper = (SocketWrapper)result.AsyncState;
    var socket = wrapper.Socket;

    try {
      var bytesRead = socket.EndReceive(result);

      wrapper.Response.Append(Encoding.ASCII.GetString(wrapper.Buffer, 0, bytesRead));

      if (!Parser.ReceivedFullResponse(wrapper.Response.ToString()))
        socket.BeginReceive(wrapper.Buffer, 0, SocketWrapper.BufferSize, 0, Receiving, wrapper);
      else {
        Console.WriteLine("Content length: {0}", Parser.GetContentLength(wrapper.Response.ToString()));

        socket.Shutdown(SocketShutdown.Both);
        socket.Close();

        wrapper.CDE.Signal();
      }
    } catch (Exception e) {
      Console.WriteLine(e);
    }
  }
}
