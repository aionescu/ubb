using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

sealed class SocketWrapper {
  public const int BufferSize = 1024;

  public Socket Socket { get; init; }

  public int ID { get; init; }
  public string HostName { get; init; }
  public string Endpoint { get; init; }
  public IPEndPoint IPEndpoint { get; init; }

  public byte[] Buffer { get; } = new byte[BufferSize];
  public StringBuilder Response { get; } = new();

  public CountdownEvent CDE { get; init; }

  public ManualResetEvent ConnectDone { get; } = new(false);
  public ManualResetEvent SendDone { get; } = new(false);
  public ManualResetEvent ReceiveDone { get; } = new(false);

  public static SocketWrapper New(string host, int id, CountdownEvent cde = null) {
    var hostName = host.Split('/')[0];
    var hostEntry = Dns.GetHostEntry(hostName);
    var ipAddr = hostEntry.AddressList[0];
    var remoteEndpoint = new IPEndPoint(ipAddr, Parser.Port);

    var socket = new Socket(ipAddr.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

    return new SocketWrapper {
      Socket = socket,
      HostName = hostName,
      Endpoint = host.Contains('/') ? host[host.IndexOf('/') ..] : "/",
      IPEndpoint = remoteEndpoint,
      ID = id,
      CDE = cde
    };
  }
}
