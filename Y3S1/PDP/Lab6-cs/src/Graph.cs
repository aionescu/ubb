namespace Lab6;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

sealed record Graph {
  public int NodeCount { get; }
  public List<int>[] Edges { get; }

  public Graph(int nodeCount) {
    NodeCount = nodeCount;
    Edges = new List<int>[nodeCount];

    for (var i = 0; i < nodeCount; ++i)
      Edges[i] = new();
  }

  static void Shuffle<A>(Random r, A[] a) {
    int n = a.Length;
    while (n > 1) {
      int k = r.Next(n--);
      (a[n], a[k]) = (a[k], a[n]);
    }
  }

  public static Graph RandomHamiltonian(int nodeCount) {
    var g = new Graph(nodeCount);

    var r = new Random();
    var nodes = Enumerable.Range(0, nodeCount).ToArray();

    Shuffle(r, nodes);

    for (var i = 0; i < nodeCount - 1; ++i)
      g.Edges[nodes[i]].Add(nodes[i + 1]);

    g.Edges[nodes[nodeCount - 1]].Add(nodes[0]);

    for (var i = 0; i < nodeCount / 2; ++i) {
      var a = r.Next(0, nodeCount);
      var b = r.Next(0, nodeCount);

      if (g.Edges[a].Contains(b)) {
        --i;
        continue;
      }

      g.Edges[a].Add(b);
    }

    return g;
  }

  public override string ToString() {
    var sb = new StringBuilder();

    for (var i = 0; i < NodeCount; ++i) {
      if (Edges[i].Count == 0)
        continue;

      sb
        .Append(i)
        .Append(" -> { ")
        .Append(string.Join(", ", Edges[i]))
        .AppendLine(" }");
    }

    return sb.ToString();
  }
}
