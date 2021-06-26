namespace DocsMovies.Models {
  using System;
  using System.Collections.Generic;
  using System.Linq;

  public class Author {
    private const StringSplitOptions _sso = System.StringSplitOptions.RemoveEmptyEntries | System.StringSplitOptions.TrimEntries;

    public int Id { get; set; }

    public string Name { get; set; }

    public string DocumentList { get; set; }

    public IEnumerable<int> Documents => DocumentList.Split(';', _sso).Select(int.Parse);

    public string MovieList { get; set; }

    public IEnumerable<int> Movies => MovieList.Split(';', _sso).Select(int.Parse);
  }
}
