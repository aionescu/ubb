const boardBorder = "black"
const boardBG = "white"
const snakeColor = "lightblue"
const snakeBorder = "darkblue"

let snake = [
  {x: 200, y: 200},
  {x: 190, y: 200},
  {x: 180, y: 200},
  {x: 170, y: 200},
  {x: 160, y: 200}
]

let score = 0
let changingDir = false

let foodX
let foodY

let dx = 10
let dy = 0

const board = document.getElementById("snakeboard")
const boardCtx = board.getContext("2d")

main()
genFood()

document.addEventListener("keydown", changeDir)

function recordMove(direction) {
  console.log("Doing recordMove")

  $.ajax({
    url: "http://localhost:8080/MakeMove",
    type: "POST",
    data: { direction },
    success: result => console.log("Success", result),
    error: result => console.log("Error", result)
  })
}

function gameOver() {
  console.log("Doing gameOver")

  $.ajax({
    url: "http://localhost:8080/GameOver",
    type: "POST",
    data: { score },
    success: result => console.log("Success", result),
    error: result => console.log("Error", result)
  })
}

function main() {
  if (isDead()) {
    document.getElementById("score").innerHTML = `Game Over (Score: ${score})`
    gameOver()
    return
  }

  changingDir = false

  setTimeout(() => {
    clearBoard()
    drawFood()
    moveSnake()
    drawSnake()
    main()
  },
  100)
}

function clearBoard() {
  boardCtx.fillStyle = boardBG;
  boardCtx.strokestyle = boardBorder;
  boardCtx.fillRect(0, 0, board.width, board.height);
  boardCtx.strokeRect(0, 0, board.width, board.height);
}

function drawSnake() {
  snake.forEach(drawSnakePart)
}

function drawFood() {
  boardCtx.fillStyle = "lightgreen"
  boardCtx.strokestyle = "darkgreen"
  boardCtx.fillRect(foodX, foodY, 10, 10)
  boardCtx.strokeRect(foodX, foodY, 10, 10)
}

function drawSnakePart(snakePart) {
  boardCtx.fillStyle = snakeColor
  boardCtx.strokestyle = snakeBorder
  boardCtx.fillRect(snakePart.x, snakePart.y, 10, 10)
  boardCtx.strokeRect(snakePart.x, snakePart.y, 10, 10)
}

function isDead() {
  for (let i = 4; i < snake.length; i++)
    if (snake[i].x === snake[0].x && snake[i].y === snake[0].y)
      return true

  const hitLeftWall = snake[0].x < 0
  const hitRightWall = snake[0].x > board.width - 10
  const hitToptWall = snake[0].y < 0
  const hitBotWall = snake[0].y > board.height - 10

  return hitLeftWall || hitRightWall || hitToptWall || hitBotWall
}

function randFood(min, max) {
  return Math.round((Math.random() * (max-min) + min) / 10) * 10
}

function genFood() {
  foodX = randFood(0, board.width - 10)
  foodY = randFood(0, board.height - 10)

  snake.forEach(part => {
    if (part.x == foodX && part.y == foodY)
      genFood()
  })
}

function changeDir(event) {
  const LEFT_KEY = 65
  const RIGHT_KEY = 68
  const UP_KEY = 87
  const DOWN_KEY = 83

  if (changingDir)
    return

  changingDir = true

  const keyPressed = event.keyCode

  const goingUp = dy === -10
  const goingDown = dy === 10
  const goingRight = dx === 10
  const goingLeft = dx === -10

  let moveDir

  if (keyPressed === LEFT_KEY && !goingRight) {
    dx = -10
    dy = 0
    moveDir = 0
  }

  if (keyPressed === UP_KEY && !goingDown) {
    dx = 0
    dy = -10
    moveDir = 1
  }

  if (keyPressed === RIGHT_KEY && !goingLeft) {
    dx = 10
    dy = 0
    moveDir = 2
  }

  if (keyPressed === DOWN_KEY && !goingUp) {
    dx = 0
    dy = 10
    moveDir = 3
  }

  recordMove(moveDir)
}

function moveSnake() {
  const head = { x: snake[0].x + dx, y: snake[0].y + dy }
  snake.unshift(head)

  const eatenFood = snake[0].x === foodX && snake[0].y === foodY

  if (eatenFood) {
    score += 10
    document.getElementById("score").innerHTML = `Score: ${score}`
    genFood()
  } else
    snake.pop()
}
