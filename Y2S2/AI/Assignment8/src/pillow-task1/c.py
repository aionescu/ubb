from PIL import Image # type: ignore

img = Image.open("/home/oldpug/Pictures/Wallpapers/Pandaren Monk.jpg")
print(img.size)

x, y = img.size
img.thumbnail((x // 2, y // 2))
print(img.size)

img.show()
