const initialHTML = $(".text-holder").html()

document.onselectionchange = () => {
  const word = document.getSelection().toString()
  const regex = new RegExp(`(${word})`, "g")

  $(".text-holder").html(initialHTML.replace(regex, `<span class="highlight">$1</span>`))
}
