import matplotlib.pyplot as plt  # type: ignore
from matplotlib.collections import PathCollection # type: ignore
from typing import Any, Dict, Iterable, List

from data import CentroidInputs, CentroidLabels, Label, Record, unzip_records

def plot_actual_outputs(inputs: List[Record], outputs: List[Label]) -> None:
  for lbl in Label:
    lbl_inputs = [r for i, r in enumerate(inputs) if outputs[i] == lbl]
    val1s, val2s = unzip_records(lbl_inputs)

    plt.scatter(val1s, val2s, label = "stars", color = lbl.color, marker = "o", s = 10)

def plot_centroids(centroids: Iterable[Record]) -> PathCollection:
  val1s, val2s = unzip_records(centroids)
  return plt.scatter(val1s, val2s, label = "stars", color = "black", marker = "D", s = 50)

def plot_computed_outputs(cent_inputs: CentroidInputs, cent_lbls: CentroidLabels) -> PathCollection:
  for centroid, inputs in cent_inputs.items():
    val1s, val2s = unzip_records(inputs)
    plt.scatter(val1s, val2s, label = "stars", color = cent_lbls[centroid].color, marker = "o", s = 10)

  return plot_centroids(cent_inputs.keys())

def plot_all(inputs: List[Record], outputs: List[Label], cent_inputs: CentroidInputs, cent_lbls: CentroidLabels) -> PathCollection:
  plt.subplot(1, 2, 2)
  plt.title("Actual outputs")
  plot_actual_outputs(inputs, outputs)

  plt.subplot(1, 2, 1)
  plt.title("Computed outputs")
  return plot_computed_outputs(cent_inputs, cent_lbls)
