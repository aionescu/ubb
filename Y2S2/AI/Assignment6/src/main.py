#!/usr/bin/env python3

from csv import reader
from enum import Enum
from typing import List, Any, NamedTuple

class Label(Enum):
  A = 0
  B = 1
  C = 2
  D = 3

class Record(NamedTuple):
  label: Label
  val1: float
  val2: float

  def __repr__(self) -> str:
    return f"[{self.label.name}, {self.val1}, {self.val2}]"

def parse_csv(path: str) -> List[List[str]]:
  with open(path) as f:
    return list(reader(f))

def parse_record(row: List[str]) -> Record:
  return Record(Label[row[0]], float(row[1]), float(row[2]))

def parse_records(rows: List[List[str]]) -> List[Record]:
  return list(map(parse_record, rows[1:]))

def load_data(path: str) -> List[Record]:
  return parse_records(parse_csv(path))

def main() -> None:
  print(load_data("../data/dataset.csv"))

if __name__ == "__main__":
  main()
