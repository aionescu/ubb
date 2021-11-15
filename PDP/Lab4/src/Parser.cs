using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace lab4.utils {
  class Parser {
    public static int PORT = 80; // http default port

    public static string GetRequestString(string hostname, string endpoint) {
      return "GET " + endpoint + " HTTP/1.1\r\n" +
             "Host: " + hostname + "\r\n" +
             "Content-Length: 0\r\n\r\n";
    }

    public static int GetContentLen(string respContent) {
      var contentLen = 0;
      var respLines = respContent.Split('\r', '\n');
      foreach (string respLine in respLines) {
        var headDetails = respLine.Split(':');

        if (String.Compare(headDetails[0], "Content-Length", StringComparison.Ordinal) == 0) {
          contentLen = int.Parse(headDetails[1]);
        }
      }

      return contentLen;
    }

    public static bool ResponseHeaderObtained(string responseContent) {
      return responseContent.Contains("\r\n\r\n");
    }
  }
}
