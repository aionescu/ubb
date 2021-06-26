using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Exam.DataAbstraction;
using Exam.Models;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace Exam.Controllers {
  public class HomeController: Controller {
    public IActionResult Index() => View();

    public IActionResult DeletePublishingHouse() => View();

    [HttpPost]
    public IActionResult DeletePublishingHouse(string name) {
      var result = new DBDriver().DeletePublishingHouse(name ?? "");

      return result switch {
        DeletionResult.Successful => View("DeletionSuccessful"),
        DeletionResult.HasBooks => View("DeletionFailed"),
        _ => View("DeletionInvalid")
      };
    }

    public IActionResult ShowPublishingHouses() {
      DBDriver db = new();

      var houses = db.GetAllPublishingHouses();
      houses.ForEach(h => h.BookCount = db.GetBookCount(h.ID));

      ViewData["houses"] = houses;
      return View();
    }

    public IActionResult FilterBooks() => View();

    [HttpPost]
    public IActionResult FilterBooks(string topics) {
      DBDriver db = new();

      var ts = (topics ?? "").Split(';', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
      var books = db.GetAllBooks().Where(b => b.Topics.Intersect(ts).Count() == 3).ToArray();

      ViewData["books"] = books;
      return View("ShowBooks");
    }

    [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
    public IActionResult Error() {
      return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
    }
  }
}
