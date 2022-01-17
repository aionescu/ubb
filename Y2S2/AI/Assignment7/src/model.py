from torch.nn import Module, Linear # type: ignore
from torch.nn.functional import relu
from typing import Any

class Net(Module): # type: ignore
  def __init__(self, feature: int = 2, hidden: int = 200, output: int = 1) -> None:
    super(Net, self).__init__()
    self.hidden = Linear(feature, hidden)
    self.output = Linear(hidden, output)

  def forward(self, x: Any) -> Any:
    return self.output(relu(self.hidden(x)))
