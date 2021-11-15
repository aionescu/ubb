using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using lab4.impl;

namespace lab4 {
  class Program {
    static void Main(string[] args) {
      var hosts = new string[] { "www.cs.ubbcluj.ro/~motogna/LFTC", "www.cs.ubbcluj.ro/~rlupsa/edu/pdp/", "www.cs.ubbcluj.ro/~forest" }.ToList();
      TaskImpl.run(hosts, false);
      //CallbackImpl.run(hosts);
    }
  }
}
