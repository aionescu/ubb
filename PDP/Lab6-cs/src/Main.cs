using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using Lab6;

static A timed<A>(Func<A> f, [CallerArgumentExpression("f")] string? name = null) {
  var sw = Stopwatch.StartNew();
  var r = f();

  var elapsed = sw.Elapsed;
  Console.WriteLine($"{name ?? "Action"} took {elapsed}");
  return r;
}

var g = Graph.RandomHamiltonian(200);
g.Edges[^1].Clear();

var sol = new List<int>();
Console.WriteLine(Solver.Visit(g, 0, ref sol, 0));
