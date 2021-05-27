from typing import Any, List
from torch import device, cuda # type: ignore
from torch.utils.data import Dataset
from torchvision import transforms # type: ignore
from PIL.Image import Image # type: ignore

device = device("cuda:0" if cuda.is_available() else "cpu")

class ImageClassifierDataset(Dataset[Any]):
  def __init__(self, imgs: List[Image], img_classes: List[str]) -> None:
    self.classes = list(set(img_classes))
    self.cls_to_lbl = { "face": 1, "nonface": 0 }

    self.img_size = 224
    self.transforms = transforms.Compose([
      transforms.Resize(self.img_size),
      transforms.CenterCrop(self.img_size),
      transforms.ToTensor(),
      transforms.Normalize((0.5, 0.5, 0.5), (0.5, 0.5, 0.5))
    ])

    self.imgs = list(map(self.transforms, imgs))
    self.lbls = list(map(lambda c: self.cls_to_lbl[c], img_classes))

  def __getitem__(self, idx: int) -> Any:
    return self.imgs[idx], self.lbls[idx]

  def __len__(self) -> int:
    return len(self.imgs)
