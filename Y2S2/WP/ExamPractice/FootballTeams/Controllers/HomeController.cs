using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using FootballTeams.DataAbstraction;
using FootballTeams.Models;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace FootballTeams.Controllers {
  public class HomeController: Controller {
    private string PlayerName {
      get => HttpContext.Session.GetString("name");
      set => HttpContext.Session.SetString("name", value);
    }

    public IActionResult Index() => PlayerName is null ? RedirectToAction("Login") : View();

    public IActionResult Login() => View();

    [HttpPost]
    public IActionResult Login(string name) {
      PlayerName = name;
      return RedirectToAction("Index");
    }

    public IActionResult AllTeams() {
      ViewData["teams"] = new DBDriver().GetAllTeams();
      return View();
    }

    public IActionResult MyTeams() {
      DBDriver db = new();

      var player = db.GetPlayer(PlayerName);
      var teams = db.GetAllTeams().Where(t => t.Members.Contains(player.ID)).ToList();

      ViewData["teams"] = teams;
      return View();
    }

    public IActionResult AssignPlayer() => View();

    [HttpPost]
    public IActionResult AssignPlayer(string player, string teams) {
      if (player == PlayerName)
        return RedirectToAction("Index");

      DBDriver db = new();

      Team makeTeam(string name) {
        Team team = new() { Name = name };
        db.AddTeam(team);
        return team;
      }

      var p = db.GetPlayer(player);

      if (p is null)
        return RedirectToAction("Index");

      teams.Split(";", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
      .Select(t => db.GetTeam(t) ?? makeTeam(t))
      .ToList()
      .ForEach(t => {
        t.MembersString += $";{p.ID}";
        db.UpdateTeam(t);
      });

      return RedirectToAction("Index");
    }

    [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
    public IActionResult Error() {
      return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
    }
  }
}
