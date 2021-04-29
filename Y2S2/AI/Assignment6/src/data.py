from csv import reader
from enum import Enum
from typing import Dict, Iterable, List, NamedTuple, Tuple

class Label(Enum):
  A = 0
  B = 1
  C = 2
  D = 3

  @property
  def color(self) -> str:
    return colors[self]

  def __repr__(self) -> str:
    return self.name

colors: Dict[Label, str] = {
  Label.A: "green",
  Label.B: "blue",
  Label.C: "yellow",
  Label.D: "red"
}

class Record(NamedTuple):
  val1: float
  val2: float

  def __repr__(self) -> str:
    return f"({self.val1}, {self.val2})"

def unzip_records(rs: Iterable[Record]) -> Tuple[List[float], List[float]]:
  val1s = []
  val2s = []

  for val1, val2 in rs:
    val1s.append(val1)
    val2s.append(val2)

  return val1s, val2s

CentroidInputs = Dict[Record, List[Record]]
CentroidLabels = Dict[Record, Label]

def parse_csv(path: str) -> List[List[str]]:
  with open(path) as f:
    return list(reader(f))

def parse_record(row: List[str]) -> Tuple[Record, Label]:
  return Record(float(row[1]), float(row[2])), Label[row[0]]

def parse_records(rows: List[List[str]]) -> Tuple[List[Record], List[Label]]:
  data = list(map(parse_record, rows[1:]))
  return [record for record, _ in data], [label for _, label in data]

def load_data(path: str) -> Tuple[List[Record], List[Label]]:
  return parse_records(parse_csv(path))
