#!/usr/bin/env python3

from torch import sum, max, eq, save, cuda # type: ignore
from torch.nn import CrossEntropyLoss # type: ignore
from torch.nn import Module, Conv2d, BatchNorm2d, ReLU, MaxPool2d, AvgPool2d, Sequential, Linear # type: ignore
from torch.optim.optimizer import Optimizer
from torch.utils.data import DataLoader
from torch.optim import Adam
from torch.autograd import Variable
from torch.tensor import Tensor
from dataset import Datum, ImageClassifierDataset
from os import listdir
from PIL import Image # type: ignore

class Unit(Module): # type: ignore
  def __init__(self, in_channels: int, out_channels: int) -> None:
    super(Unit, self).__init__()

    self.conv = Conv2d(in_channels = in_channels, kernel_size = 3, out_channels = out_channels, stride = 1, padding = 1)
    self.bn = BatchNorm2d(num_features=out_channels)
    self.relu = ReLU()

  def forward(self, input: Tensor) -> Tensor:
    return self.relu(self.bn(self.conv(input))) # type: ignore

class SimpleNet(Module): # type: ignore
  def __init__(self, num_classes: int = 10) -> None:
    super(SimpleNet, self).__init__()

    self.net = Sequential(
      Unit(in_channels = 3, out_channels = 32),
      Unit(in_channels = 32, out_channels = 32),
      Unit(in_channels = 32, out_channels = 32),

      MaxPool2d(kernel_size = 2),

      Unit(in_channels = 32, out_channels = 64),
      Unit(in_channels = 64, out_channels = 64),
      Unit(in_channels = 64, out_channels = 64),
      Unit(in_channels = 64, out_channels = 64),

      MaxPool2d(kernel_size = 2),

      Unit(in_channels = 64, out_channels = 128),
      Unit(in_channels = 128, out_channels = 128),
      Unit(in_channels = 128, out_channels = 128),
      Unit(in_channels = 128, out_channels = 128),

      MaxPool2d(kernel_size = 2),

      Unit(in_channels = 128, out_channels = 128),
      Unit(in_channels = 128, out_channels = 128),
      Unit(in_channels = 128, out_channels = 128),

      AvgPool2d(kernel_size = 4)
    )

    self.fc = Linear(in_features = 128 * 7 * 7, out_features = num_classes)

  def forward(self, input: Tensor) -> Tensor:
    output = self.net(input)
    output = output.view(output.size(0), -1)
    return self.fc(output) # type: ignore

batch_size = 16

def load_data(path: str) -> DataLoader[Datum]:
  faces_path = path + "/faces/"
  nonfaces_path = path + "/nonfaces/"

  face_files = [faces_path + f for f in listdir(faces_path)]
  nonface_files = [nonfaces_path + f for f in listdir(nonfaces_path)]

  clss = ["face"] * len(face_files) + ["nonface"] * len(nonface_files)
  imgs = list(map(Image.open, face_files)) + list(map(Image.open, nonface_files))

  dataset = ImageClassifierDataset(imgs, clss)
  return DataLoader(dataset, batch_size = batch_size, shuffle = True, num_workers = 4)

def adjust_learning_rate(optimizer: Optimizer, epoch: int) -> None:
  adjustment = 10 ** (epoch // 30)
  lr = 0.001 / adjustment

  for param_group in optimizer.param_groups:
    param_group["lr"] = lr

def save_model(model: SimpleNet, epoch: int, acc: float) -> None:
  save(model.state_dict(), f"../model/epoch{epoch}-acc{acc:.2f}.model")
  print(f"Model saved (Epoch {epoch})")

def test(model: SimpleNet, use_cuda: bool, loader: DataLoader[Datum]) -> float:
  model.eval()
  test_acc = 0.0

  for img, lbl in loader:
    if use_cuda:
      img = Variable(img.cuda())
      lbl = Variable(lbl.cuda())

    outputs = model(img)
    _, prediction = max(outputs.data, 1)

    test_acc += sum(eq(prediction, lbl.data))

  return test_acc / len(loader.dataset) # type: ignore

def train(model: SimpleNet, use_cuda: bool, train_loader: DataLoader[Datum], test_loader: DataLoader[Datum], num_epochs: int) -> None:
  optimizer = Adam(model.parameters(), lr = 0.001, weight_decay = 0.0001)
  loss_fn = CrossEntropyLoss()
  best_acc = 0.0

  for epoch in range(num_epochs):
    model.train()
    train_acc = 0.0
    train_loss = 0.0

    for images, labels in train_loader:
      if use_cuda:
        images = Variable(images.cuda())
        labels = Variable(labels.cuda())

      optimizer.zero_grad() # Clear all accumulated gradients
      outputs = model(images) # Predict classes using images from the test set
      loss = loss_fn(outputs,labels) # Compute the loss based on the predictions and actual labels
      loss.backward() # Backpropagate the loss

      optimizer.step() # Adjust parameters according to the computed gradients

      train_loss += loss.cpu().data.item() * images.size(0)
      _, prediction = max(outputs.data, 1)

      train_acc += sum(prediction == labels.data)

    adjust_learning_rate(optimizer, epoch)

    data_size = len(train_loader.dataset) # type: ignore
    train_acc = train_acc / data_size
    train_loss = train_loss / data_size

    test_acc = test(model, use_cuda, test_loader)

    print(f"Epoch: {epoch}, Train Accuracy: {train_acc}, Train Loss: {train_loss}, Test Accuracy: {test_acc}")

    if test_acc > best_acc:
      save_model(model, epoch, test_acc)
      best_acc = test_acc

def main() -> None:
  train_loader = load_data("../data/train")
  test_loader = load_data("../data/test")

  use_cuda = cuda.is_available()

  model = SimpleNet(num_classes = 10)

  if use_cuda:
    model.cuda()

  train(model, use_cuda, train_loader, test_loader, 50)

if __name__ == "__main__":
  main()
