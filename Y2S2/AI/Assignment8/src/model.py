from typing import Any
import torch
import torch.nn as nn
from torch.nn import Module, Conv2d, BatchNorm2d, ReLU, MaxPool2d, AvgPool2d, Sequential, Linear # type: ignore
from torchvision.datasets import CIFAR10 # type: ignore
from torchvision.transforms import transforms # type: ignore
from torch.utils.data import DataLoader
from torch.optim import Adam
from torch.autograd import Variable
from dataset import ImageClassifierDataset
from os import listdir
from PIL import Image

class Unit(Module): # type: ignore
  def __init__(self, in_channels: int, out_channels: int) -> None:
    super(Unit, self).__init__()

    self.conv = Conv2d(in_channels = in_channels, kernel_size = 3,out_channels = out_channels, stride = 1, padding = 1)
    self.bn = BatchNorm2d(num_features=out_channels)
    self.relu = ReLU()

  def forward(self, input: Any) -> Any:
    return self.relu(self.bn(self.conv(input)))

class SimpleNet(Module): # type: ignore
  def __init__(self, num_classes: int = 10) -> None:
    super(SimpleNet,self).__init__()

    self.net = Sequential(
      Unit(in_channels=3,out_channels=32),
      Unit(in_channels=32, out_channels=32),
      Unit(in_channels=32, out_channels=32),

      MaxPool2d(kernel_size=2),

      Unit(in_channels=32, out_channels=64),
      Unit(in_channels=64, out_channels=64),
      Unit(in_channels=64, out_channels=64),
      Unit(in_channels=64, out_channels=64),

      MaxPool2d(kernel_size=2),

      Unit(in_channels=64, out_channels=128),
      Unit(in_channels=128, out_channels=128),
      Unit(in_channels=128, out_channels=128),
      Unit(in_channels=128, out_channels=128),

      MaxPool2d(kernel_size=2),

      Unit(in_channels=128, out_channels=128),
      Unit(in_channels=128, out_channels=128),
      Unit(in_channels=128, out_channels=128),

      AvgPool2d(kernel_size=4)
    )

    self.fc = Linear(in_features = 128 * 7 * 7, out_features = num_classes)

  def forward(self, input):
    output = self.net(input)
    output = output.view(output.size(0), -1)
    return self.fc(output)

train_transformations = transforms.Compose([
  transforms.RandomHorizontalFlip(),
  transforms.RandomCrop(32,padding=4),
  transforms.ToTensor(),
  transforms.Normalize((0.5, 0.5, 0.5), (0.5, 0.5, 0.5))
])

batch_size = 16

TRAIN_FACES = "../img/train-faces/"
TRAIN_NONFACES = "../img/train-nonfaces/"
TEST_FACES = "../img/test-faces/"
TEST_NONFACES = "../img/test-nonfaces/"

face_files = [TRAIN_FACES + f for f in listdir(TRAIN_FACES)]
nonface_files = [TRAIN_NONFACES + f for f in listdir(TRAIN_NONFACES)]

classes = ["face"] * len(face_files) + ["nonface"] * len(nonface_files)
imgs = list(map(Image.open, face_files)) + list(map(Image.open, nonface_files))

train_set = ImageClassifierDataset(imgs, classes)
# train_set = CIFAR10(root="./data",train=True,transform=train_transformations,download=True)

train_loader = DataLoader(train_set, batch_size = batch_size, shuffle = True, num_workers = 4)

#Define transformations for the test set
test_transformations = transforms.Compose([
  transforms.ToTensor(),
  transforms.Normalize((0.5,0.5,0.5), (0.5,0.5,0.5))
])

face_files = [TEST_FACES + f for f in listdir(TEST_FACES)]
nonface_files = [TEST_NONFACES + f for f in listdir(TEST_NONFACES)]

classes = ["face"] * len(face_files) + ["nonface"] * len(nonface_files)
imgs = list(map(Image.open, face_files)) + list(map(Image.open, nonface_files))

#Load the test set, note that train is set to False
test_set = ImageClassifierDataset(imgs, classes)
# test_set = CIFAR10(root="./data",train=False,transform=test_transformations,download=True)

#Create a loder for the test set, note that both shuffle is set to false for the test loader
test_loader = DataLoader(test_set,batch_size=batch_size,shuffle=False,num_workers=4)

#Check if gpu support is available
cuda_avail = torch.cuda.is_available()

#Create model, optimizer and loss function
model = SimpleNet(num_classes = 10)

if cuda_avail:
  model.cuda()

optimizer = Adam(model.parameters(), lr=0.001,weight_decay=0.0001)
loss_fn = nn.CrossEntropyLoss()

#Create a learning rate adjustment function that divides the learning rate by 10 every 30 epochs
def adjust_learning_rate(epoch):
  lr = 0.001

  if epoch > 180:
    lr = lr / 1000000
  elif epoch > 150:
    lr = lr / 100000
  elif epoch > 120:
    lr = lr / 10000
  elif epoch > 90:
    lr = lr / 1000
  elif epoch > 60:
    lr = lr / 100
  elif epoch > 30:
    lr = lr / 10

  for param_group in optimizer.param_groups:
    param_group["lr"] = lr

def save_model(epoch, acc):
  torch.save(model.state_dict(), f"../model/epoch{epoch}-acc{acc:.2f}.model")
  print(f"Model saved (Epoch {epoch})")

def test():
  model.eval()
  test_acc = 0.0

  for i, (images, labels) in enumerate(test_loader):
    if cuda_avail:
      images = Variable(images.cuda())
      labels = Variable(labels.cuda())

    #Predict classes using images from the test set
    outputs = model(images)
    _,prediction = torch.max(outputs.data, 1)
    #prediction = prediction.cpu().numpy()

    test_acc += torch.sum(torch.eq(prediction, labels.data))

  #Compute the average acc and loss over all 10000 test images
  test_acc = test_acc / len(test_set)

  return test_acc

def train(num_epochs):
    best_acc = 0.0

    for epoch in range(num_epochs):
        model.train()
        train_acc = 0.0
        train_loss = 0.0
        for i, (images, labels) in enumerate(train_loader):
            #Move images and labels to gpu if available
            if cuda_avail:
                images = Variable(images.cuda())
                labels = Variable(labels.cuda())

            #Clear all accumulated gradients
            optimizer.zero_grad()
            #Predict classes using images from the test set
            outputs = model(images)
            #Compute the loss based on the predictions and actual labels
            loss = loss_fn(outputs,labels)
            #Backpropagate the loss
            loss.backward()

            #Adjust parameters according to the computed gradients
            optimizer.step()


            train_loss += loss.cpu().data.item() * images.size(0)
            _, prediction = torch.max(outputs.data, 1)

            train_acc += torch.sum(prediction == labels.data)

        #Call the learning rate adjustment function
        adjust_learning_rate(epoch)

        #Compute the average acc and loss over all 50000 training images
        train_acc = train_acc / len(train_set)
        train_loss = train_loss / len(train_set)

        #Evaluate on the test set
        test_acc = test()

        print(f"Epoch: {epoch}, Train Accuracy: {train_acc}, Train Loss: {train_loss}, Test Accuracy: {test_acc}")

        if test_acc > best_acc:
          save_model(epoch, test_acc)
          best_acc = test_acc

if __name__ == "__main__":
  train(50)
