for (let i = 1; i <= 8; ++i) {
  const img = document.createElement("img")

  img.src = `Assets/${i}.jpg`
  img.width = 1920 / 8.2
  img.height = 1080 / 8.2

  img.onmouseover = () => document.getElementById("bigImage").src = img.src

  document.body.appendChild(img)
}

const bigImg = document.createElement("img")
bigImg.id = "bigImage"

document.body.appendChild(bigImg)
