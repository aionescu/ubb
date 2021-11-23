using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

sealed class SocketWrapper {
  public const int BufferSize = 1024;

  public Socket Socket;

  public int ID;
  public string HostName;
  public string Endpoint;
  public IPEndPoint IPEndpoint;

  public byte[] Buffer = new byte[BufferSize];
  public StringBuilder Response = new();

  public ManualResetEvent ConnectDone = new(false);
  public ManualResetEvent SendDone = new(false);
  public ManualResetEvent ReceiveDone = new(false);

  public CountdownEvent CDE;
}
