from matplotlib import image, pyplot # type: ignore

data = image.imread("/home/oldpug/Pictures/Wallpapers/Pandaren Monk.jpg")

print(data.dtype)
print(data.shape)

pyplot.imshow(data)
pyplot.show()
