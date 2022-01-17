from PIL import Image # type: ignore

img = Image.open("/home/oldpug/Pictures/Wallpapers/Pandaren Monk.jpg")

print(img.format)
print(img.mode)
print(img.size)

img.show()
