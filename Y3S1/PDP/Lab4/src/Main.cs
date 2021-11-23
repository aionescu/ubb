using System.Collections.Generic;

List<string> hosts = new() {
  "cs.ubbcluj.ro/~rlupsa/edu/pdp",
  "math.ubbcluj.ro/~crivei/",
  "haskell.org",
  "cataas.com/cat/12",
};

//CallbackImpl.Run(hosts);
TaskImpl.Run(hosts, false);
//TaskImpl.Run(hosts, true);
