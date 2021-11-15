using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using lab4.domain;
using lab4.utils;

namespace lab4.impl {
  class CallbackImpl {

    public static void run(List<string> hostnames) {
      for (var i = 0; i < hostnames.Count; i++) {
        StartClient(hostnames[i], i);
        Thread.Sleep(1000);
      }
    }

    private static void StartClient(string host, int id) {
      var ipHostInfo = Dns.GetHostEntry(host.Split('/')[0]); // get host dns entry
      var ipAddress = ipHostInfo.AddressList[0]; // separate ip of host
      var remoteEndpoint = new IPEndPoint(ipAddress, Parser.PORT); // create endpoint

      var client = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp); // create client socket

      var requestSocket = new CustomSocket {
        sock = client,
        hostname = host.Split('/')[0],
        endpoint = host.Contains("/") ? host.Substring(host.IndexOf("/")) : "/",
        remoteEndPoint = remoteEndpoint,
        id = id
      }; // build a socket

      requestSocket.sock.BeginConnect(requestSocket.remoteEndPoint, Connected, requestSocket); // connect to the remote endpoint
    }

    private static void Connected(IAsyncResult ar) {
      var resultSocket = (CustomSocket)ar.AsyncState; // conn state
      var clientSocket = resultSocket.sock;
      var clientId = resultSocket.id;
      var hostname = resultSocket.hostname;

      clientSocket.EndConnect(ar); // end connection
      Console.WriteLine("Connection {0} > Socket connected to {1} ({2})", clientId, hostname, clientSocket.RemoteEndPoint);

      var byteData = Encoding.ASCII.GetBytes(Parser.GetRequestString(resultSocket.hostname, resultSocket.endpoint));

      resultSocket.sock.BeginSend(byteData, 0, byteData.Length, 0, Sent, resultSocket);
    }

    private static void Sent(IAsyncResult ar) {
      var resultSocket = (CustomSocket)ar.AsyncState;
      var clientSocket = resultSocket.sock;
      var clientId = resultSocket.id;

      // send data to server
      var bytesSent = clientSocket.EndSend(ar);
      Console.WriteLine("Connection {0} > Sent {1} bytes to server.", clientId, bytesSent);

      // server response (data)
      resultSocket.sock.BeginReceive(resultSocket.buffer, 0, CustomSocket.BUFF_SIZE, 0, Receiving, resultSocket);
    }

    private static void Receiving(IAsyncResult ar) {
      // get answer details
      var resultSocket = (CustomSocket)ar.AsyncState;
      var clientSocket = resultSocket.sock;
      var clientId = resultSocket.id;

      try {
        var bytesRead = clientSocket.EndReceive(ar); // read response data

        resultSocket.responseContent.Append(Encoding.ASCII.GetString(resultSocket.buffer, 0, bytesRead));

        // if the response header has not been fully obtained, get the next chunk of data
        if (!Parser.ResponseHeaderObtained(resultSocket.responseContent.ToString())) {
          clientSocket.BeginReceive(resultSocket.buffer, 0, CustomSocket.BUFF_SIZE, 0, Receiving, resultSocket);
        } else {
          Console.WriteLine("Content length is:{0}", Parser.GetContentLen(resultSocket.responseContent.ToString()));

          clientSocket.Shutdown(SocketShutdown.Both); // free socket
          clientSocket.Close();
        }
      } catch (Exception e) {
        Console.WriteLine(e.ToString());
      }
    }
  }
}
