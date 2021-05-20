from math import sin,  pi
from torch import rand, save, Tensor

DATA_PATH = "../data/data_set.dat"
NN_PATH = "../data/net.pt"

def f(x: float, y: float) -> float:
  return sin(x + y / pi)

def main() -> None:
  inputs = rand(5000, 2) * 20 - 10
  outputs = Tensor(list(map(lambda t: f(*t), inputs))).unsqueeze(1)
  save((inputs, outputs), DATA_PATH)

if __name__ == "__main__":
  main()
