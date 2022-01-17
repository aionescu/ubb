import matplotlib.pyplot as plt  # type: ignore
from matplotlib.collections import PathCollection # type: ignore

from data import CentroidInputs, CentroidLabels, unzip_records

def draw_plot(cent_inputs: CentroidInputs, cent_lbls: CentroidLabels) -> PathCollection:
  for centroid, inputs in cent_inputs.items():
    val1s, val2s = unzip_records(inputs)
    plt.scatter(val1s, val2s, label = "points", color = cent_lbls[centroid].color, marker = "o", s = 10)

  val1s, val2s = unzip_records(cent_inputs.keys())
  return plt.scatter(val1s, val2s, label = "stars", color = "black", marker = "D", s = 50)
