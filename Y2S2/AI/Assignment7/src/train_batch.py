import matplotlib.pyplot as plt # type: ignore
from create_db import DATA_PATH, NN_PATH
from model import Net
from torch import load, save
from torch.nn import MSELoss # type: ignore
from torch.optim import SGD

(x, y) = load(DATA_PATH) # type: ignore

loss_fn = MSELoss()
ann = Net()

optimizer_batch = SGD(ann.parameters(), lr = 0.0005)
loss_list = []

batch_size = 50
batch_count = int(len(x) / batch_size)

for epoch in range(1, 2001):
  for batch in range(batch_count):
    batch_begin = batch * batch_size
    batch_end = (batch + 1) * batch_size

    batch_x, batch_y = x[batch_begin:batch_end], y[batch_begin:batch_end]

    prediction = ann(batch_x)
    loss = loss_fn(prediction, batch_y)

    optimizer_batch.zero_grad()
    loss.backward()

    optimizer_batch.step()

  if epoch % 100 == 0:
    loss_list.append(loss.tolist())
    y_pred = ann(x)
    loss = loss_fn(y_pred, y)

    print(f"Epoch: {epoch}, Loss: {loss:.5f}")

save(ann.state_dict(), NN_PATH)

plt.plot(loss_list)
plt.show()
