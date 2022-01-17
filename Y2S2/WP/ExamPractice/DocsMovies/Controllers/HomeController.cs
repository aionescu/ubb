using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using DocsMovies.DataAbstraction;
using DocsMovies.Models;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace DocsMovies.Controllers {
  public class HomeController: Controller {
    private string AuthorName {
      get => HttpContext.Session.GetString("name");
      set => HttpContext.Session.SetString("name", value);
    }

    public IActionResult Index() {
      if (AuthorName is null)
        return RedirectToAction("Login");

      return View();
    }

    public IActionResult Login() => View();

    [HttpPost]
    public IActionResult Login(string name) {
      AuthorName = name;
      return RedirectToAction("Index");
    }

    public IActionResult Privacy() {
      return View();
    }

    public IActionResult AddDoc() {
      return View();
    }

    [HttpPost]
    public IActionResult AddDoc(string name, string contents) {
      Console.WriteLine("Name: " + name);
      Console.WriteLine("Contents: " + contents);

      new DbDriver().AddDocument(new() { Name = name, Contents = contents });

      return RedirectToAction("Index");
    }

    public IActionResult GetDocs() {
      DbDriver db = new();

      var author = db.GetAuthor(AuthorName);

      var docs = author.Documents.Select(db.GetDocument).ToArray();
      var movies = author.Movies.Select(db.GetMovie).ToArray();

      var rows =
        docs.Zip(movies, (doc, movie) => new[] {
          (doc.Id, doc.Name, doc.Contents, "", ""),
          (movie.Id, "", "", movie.Title, movie.Duration.ToString())
        })
        .SelectMany(r => r)
        .ToList();

      if (docs.Length > movies.Length)
        rows.AddRange(docs[movies.Length ..].Select(doc => (doc.Id, doc.Name, doc.Contents, "", "")));
      else
        rows.AddRange(movies[docs.Length ..].Select(movie => (movie.Id, "", "", movie.Title, movie.Duration.ToString())));

      ViewData["rows"] = rows;

      return View();
    }

    public IActionResult BestDoc() {
      DbDriver db = new();
      var authors = db.GetAllAuthors();

      Dictionary<int, int> docs = new();

      foreach (var author in authors)
        foreach (var doc in author.Documents)
          docs[doc] = docs.TryGetValue(doc, out var c) ? c + 1 : 1;

      var bestDoc = docs.OrderByDescending(d => d.Value).First();

      ViewData["doc"] = db.GetDocument(bestDoc.Key);
      ViewData["count"] = bestDoc.Value;

      return View();
    }

    [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
    public IActionResult Error() {
      return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
    }
  }
}
