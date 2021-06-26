using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Vacations.DataAbstraction;
using Vacations.Models;

namespace Vacations.Controllers {
  public class HomeController: Controller {
    private string Username {
      get => HttpContext.Session.GetString("username");
      set => HttpContext.Session.SetString("username", value);
    }

    public IActionResult Index() {
      if (Username is null)
        return RedirectToAction("Login");

      return View();
    }

    public IActionResult Login() => View();

    [HttpPost]
    public IActionResult Login(string username) {
      Username = username;
      return RedirectToAction("Index");
    }

    public IActionResult Ban() => View();

    [HttpPost]
    public IActionResult Ban(string destination) {
      new DBDriver().Ban(Username, destination);
      return RedirectToAction("Index");
    }

    public IActionResult Destinations() {
      ViewData["dests"] = GetDestinations("");
      return View();
    }

    [HttpGet]
    public VacationDestination[] GetDestinations(string filter) {
      var db = new DBDriver();

      return
        db.GetAllDestinations()
        .Where(d =>
          d.Destination.Contains(filter ?? "", StringComparison.OrdinalIgnoreCase)
          && !db.IsBanned(Username, d.ID))
        .ToArray();
    }

    [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
    public IActionResult Error() {
      return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
    }
  }
}
