import matplotlib.pyplot as plt  # type: ignore
from math import sqrt
from random import uniform
from typing import Dict, List, Set, Tuple

from data import CentroidInputs, CentroidLabels, Label, Record, unzip_records
from plot import plot_all

def gen_centroid_labels(cent_inputs: CentroidInputs) -> CentroidLabels:
  centroids = list(cent_inputs.keys())

  left = min(centroids, key = lambda x: x.val1)
  centroids.remove(left)

  right = max(centroids, key = lambda x: x.val1)
  centroids.remove(right)

  bot = min(centroids, key = lambda x: x.val2)
  centroids.remove(bot)

  top = centroids[0]

  return { left: Label.A, top: Label.B, right: Label.C, bot: Label.D }

def get_domain(inputs: List[Record]) -> Tuple[Record, Record]:
  val1s, val2s = unzip_records(inputs)
  return Record(min(val1s), min(val2s)), Record(max(val1s), max(val2s))

def gen_centroids(domain: Tuple[Record, Record], centroid_count: int = 4) -> Set[Record]:
  (x_bot, y_bot), (x_top, y_top) = domain
  return { Record(uniform(x_bot, x_top), uniform(y_bot, y_top)) for _ in range(centroid_count) }

def euclidean(a: Record, b: Record) -> float:
  x_diff = b.val1 - a.val1
  y_diff = b.val2 - a.val2
  return sqrt(x_diff * x_diff + y_diff * y_diff)

Stat = Dict[Label, float]
Stats = Tuple[float, Stat, Stat, Stat]

def eval_classification(inputs: List[Record], actual: List[Label], computed: List[Label]) -> Stats:
  accuracy = len(list(filter(lambda p: p[0] == p[1], zip(actual, computed)))) / len(inputs)

  precision: Stat = {}
  rappel: Stat = {}

  for lbl in Label:
    correct = len(list(filter(lambda p: p[0] == p[1] == lbl, zip(actual, computed))))

    total_computed = len(list(filter(lambda l: l == lbl, computed)))
    precision[lbl] = correct / total_computed

    total_actual = len(list(filter(lambda l: l == lbl, actual)))
    rappel[lbl] = correct / total_actual

  score = { lbl: 2 * p * r / (p + r) for (lbl, p), r in zip(precision.items(), rappel.values()) }
  return accuracy, precision, rappel, score

def show_stats(inputs: List[Record], outputs: List[Label], cent_inputs: CentroidInputs) -> None:
  cent_lbls = gen_centroid_labels(cent_inputs)
  input_computed_lbls = { input: cent_lbls[cent] for cent, set_ in cent_inputs.items() for input in set_ }
  computed_lbls = [input_computed_lbls[input] for input in inputs]

  accuracy, precision, rappel, score = eval_classification(inputs, outputs, computed_lbls)
  print(f"Accuracy: {accuracy}")
  print(f"Precision: {precision}")
  print(f"Rappel: {rappel}")
  print(f"Score: {score}")

def solver(centroid_set: Set[Record], inputs: List[Record], outputs: List[Label]) -> CentroidInputs:
  centroids = list(centroid_set)
  cent_inputs: CentroidInputs = { cent: [] for cent in centroids }
  changed = True

  while changed:
    changed = False
    input_cent = { r: min(centroids, key = lambda c: euclidean(r, c)) for r in inputs }
    cent_inputs = { cent: [r for r, c in input_cent.items() if c == cent] for cent in centroids }

    new_centroids: List[Record] = []
    empty_centroid = False

    for rs in cent_inputs.values():
      val1s, val2s = unzip_records(rs)
      l = len(rs)

      if l != 0:
        new_centroids.append(Record(sum(val1s) / l, sum(val2s) / l))
      else:
        empty_centroid = True

    if empty_centroid:
      new_centroids += gen_centroids(get_domain(inputs), len(centroid_set) - len(new_centroids))

    changed = changed or any(map(lambda c: c not in centroids, new_centroids))

    if changed:
      centroids = new_centroids
    else:
      show_stats(inputs, outputs, cent_inputs)

    points = plot_all(inputs, outputs, cent_inputs, gen_centroid_labels(cent_inputs))
    plt.pause(0.5)

    if changed:
      points.remove()

  return cent_inputs
