using System.Collections.Generic;

List<string> hosts = new() {
  "www.cs.ubbcluj.ro/~motogna/LFTC",
  "www.cs.ubbcluj.ro/~rlupsa/edu/pdp/",
  "www.cs.ubbcluj.ro/~forest"
};

// CallbackImpl.Run(hosts);
// TaskImpl.Run(hosts, false);
TaskImpl.Run(hosts, true);
