using System;

namespace Exam.Models {
  public class Book {
    public int ID { get; set; }

    public int IDPublishingHouse { get; set; }

    public string Name { get; set; }

    public string Topic1 { get; set; }

    public string Topic2 { get; set; }

    public string Topic3 { get; set; }

    public string Topic4 { get; set; }

    public string Topic5 { get; set; }

    public string[] Topics => new[] { Topic1, Topic2, Topic3, Topic4, Topic5 };
  }
}
