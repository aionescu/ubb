using System;
using System.Collections.Generic;

List<string> hosts = new() {
  "cs.ubbcluj.ro/~rlupsa/edu/pdp",
  "math.ubbcluj.ro/~crivei/",
  "haskell.org",
  "cataas.com/cat/12",
};

Console.WriteLine("Callbacks");
CallbackImpl.Run(hosts);

Console.WriteLine("\nTPL");
TaskImpl.Run(hosts);

Console.WriteLine("\nTPL Async");
TaskAsyncImpl.Run(hosts);
