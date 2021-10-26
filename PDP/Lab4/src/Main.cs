using System;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Threading;

var fileURI = new Uri("https://raw.githubusercontent.com/aionescu/oplang/main/Examples/LostKingdom.bf");

var client = new WebClient();
client.DownloadDataAsync(fileURI);

client.DownloadDataCompleted += (sender, args) => { Console.WriteLine(args.Result.Length); };

while (client.IsBusy)
  Thread.Sleep(100);
