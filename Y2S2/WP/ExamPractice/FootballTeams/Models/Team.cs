using System;
using System.Linq;

namespace FootballTeams.Models {
  public class Team {
    public int ID { get; set; }

    public int CaptainID { get; set; }

    public string Name { get; set; }

    public string Description { get; set; }

    public string MembersString { get; set; }

    public int[] Members =>
      MembersString.Split(";", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
      .Select(int.Parse)
      .ToArray();
  }
}
