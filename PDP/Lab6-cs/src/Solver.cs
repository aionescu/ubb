namespace Lab6;

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

static class Solver {
  public static bool Visit(Graph g, int node, ref List<int> sol, int threads = 16) {
    sol.Add(node);

    if (sol.Count == g.NodeCount)
      return g.Edges[node].Contains(sol[0]);

    if (threads < 2) {
      foreach (var adj in g.Edges[node]) {
        if (sol.Contains(adj))
          continue;

        if (Visit(g, adj, ref sol, threads))
          return true;

        sol.RemoveAt(sol.Count - 1);
      }
    } else {
      // throw new NotImplementedException();

      var adj = g.Edges[node];

      var a = new List<int>(sol);
      var b = new List<int>(sol);

      var isSol1 = 0;
      var isSol2 = 0;

      var t1 =  Task<List<int>>.Factory.StartNew(() => {
        for (var i = 0; i < adj.Count; i += 2) {
          var aTmp = new List<int>(a);

          if (a.Contains(adj[i]))
            continue;

          if (Visit(g, adj[i], ref aTmp, threads - 2)) {
            Interlocked.Exchange(ref isSol1, 1);
            a = aTmp;
            return a;
          }
        }

        return a;
      });

      for (var i = 1; i < adj.Count; i += 2) {
        var bTmp = new List<int>(b);

        if (b.Contains(adj[i]))
          continue;

        if (Visit(g, adj[i], ref bTmp, threads - 2)) {
          Interlocked.Exchange(ref isSol2, 1);
          b = bTmp;
          break;
        }
      }

      t1.Wait();
      a = t1.Result;

      if (isSol1 != 0) {
        sol = a;
        return true;
      } else if (isSol2 != 0) {
        sol = b;
        return true;
      }
    }

    return false;
  }
}
