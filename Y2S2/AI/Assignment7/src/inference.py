from torch import load, tensor # type: ignore
from create_db import NN_PATH, f
from model import Net

def main() -> None:
  ann = Net()
  ann.load_state_dict(load(NN_PATH)) # type: ignore
  ann.eval()

  while True:
    x = float(input("x: "))
    y = float(input("y: "))
    t = tensor([x, y])

    print("Expected: ", f(x, y))
    print("Predicted: ", ann(t).tolist()[0])
    print()

if __name__ == "__main__":
  main()
