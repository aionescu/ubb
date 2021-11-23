using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;

static class TaskImpl {
  private static List<string> hosts;

  public static void Run(List<string> hostnames, bool async) {
    hosts = hostnames;
    Action<string, int> fn = async ? StartAsyncClient : StartClient;

    var tasks =
      Enumerable.Range(0, hostnames.Count)
      .Select(i => Task.Factory.StartNew(() => fn(hosts[i], i)))
      .ToArray();

    Task.WaitAll(tasks);
  }

  private static void StartClient(string host, int id) {
    var wrapper = SocketWrapper.New(host, id);

    Connect(wrapper).Wait();
    Send(wrapper, Parser.RequestString(wrapper.HostName, wrapper.Endpoint)).Wait();
    Receive(wrapper).Wait();

    Console.WriteLine($"Connection {id}: Content length: {Parser.ContentLength(wrapper.Response.ToString())}");

    wrapper.Socket.Shutdown(SocketShutdown.Both);
    wrapper.Socket.Close();
  }

  private static async void StartAsyncClient(string host, int id) {
    var wrapper = SocketWrapper.New(host, id);

    await ConnectAsync(wrapper);
    await SendAsync(wrapper, Parser.RequestString(wrapper.HostName, wrapper.Endpoint));
    await ReceiveAsync(wrapper);

    Console.WriteLine($"Connection {id}: Content length: {Parser.ContentLength(wrapper.Response.ToString())}");

    wrapper.Socket.Shutdown(SocketShutdown.Both);
    wrapper.Socket.Close();
  }

  private static async Task ConnectAsync(SocketWrapper wrapper) {
    wrapper.Socket.BeginConnect(wrapper.IPEndpoint, ConnectCallback, wrapper);
    await Task.FromResult(wrapper.ConnectDone.WaitOne());
  }

  private static Task Connect(SocketWrapper wrapper) {
    wrapper.Socket.BeginConnect(wrapper.IPEndpoint, ConnectCallback, wrapper);
    return Task.FromResult(wrapper.ConnectDone.WaitOne());
  }

  private static void ConnectCallback(IAsyncResult result) {
    // retrieve the details from the connection information wrapper
    var wrapper = (SocketWrapper)result.AsyncState;
    var socket = wrapper.Socket;
    var id = wrapper.ID;
    var hostname = wrapper.HostName;

    socket.EndConnect(result);

    Console.WriteLine($"Connection {id}: Socket connected to {hostname} ({socket.RemoteEndPoint})");

    wrapper.ConnectDone.Set();
  }

  private static async Task SendAsync(SocketWrapper wrapper, string data) {
    var bytes = Encoding.ASCII.GetBytes(data);

    wrapper.Socket.BeginSend(bytes, 0, bytes.Length, 0, SendCallback, wrapper);
    await Task.FromResult(wrapper.SendDone.WaitOne());
  }

  private static Task Send(SocketWrapper wrapper, string data) {
    var bytes = Encoding.ASCII.GetBytes(data);

    wrapper.Socket.BeginSend(bytes, 0, bytes.Length, 0, SendCallback, wrapper);
    return Task.FromResult(wrapper.SendDone.WaitOne());
  }

  private static void SendCallback(IAsyncResult result) {
    var wrapper = (SocketWrapper)result.AsyncState;
    var socket = wrapper.Socket;
    var id = wrapper.ID;

    var sent = socket.EndSend(result);

    Console.WriteLine($"Connection {id}: Sent {sent} bytes to server.");

    wrapper.SendDone.Set();
  }

  private static async Task ReceiveAsync(SocketWrapper wrapper) {
    wrapper.Socket.BeginReceive(wrapper.Buffer, 0, SocketWrapper.BufferSize, 0, ReceiveCallback, wrapper);
    await Task.FromResult(wrapper.ReceiveDone.WaitOne());
  }

  private static Task Receive(SocketWrapper wrapper) {
    wrapper.Socket.BeginReceive(wrapper.Buffer, 0, SocketWrapper.BufferSize, 0, ReceiveCallback, wrapper);
    return Task.FromResult(wrapper.ReceiveDone.WaitOne());
  }

  private static void ReceiveCallback(IAsyncResult result) {
    var wrapper = (SocketWrapper)result.AsyncState;
    var socket = wrapper.Socket;

    try {
      var bytes = socket.EndReceive(result);
      wrapper.Response.Append(Encoding.ASCII.GetString(wrapper.Buffer, 0, bytes));

      if (!Parser.ReceivedFullResponse(wrapper.Response.ToString()))
        socket.BeginReceive(wrapper.Buffer, 0, SocketWrapper.BufferSize, 0, ReceiveCallback, wrapper);
      else
        wrapper.ReceiveDone.Set();
    } catch (Exception e) {
      Console.WriteLine(e);
    }
  }
}
