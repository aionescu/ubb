param([string] $src)

$relSrc = ($PSScriptRoot + "/" + $src)
if (Test-Path $relSrc) {
  $src = $relSrc
}

if (Test-Path $src) {
  $result = & mypy $src

  if (!$?) {
    $result | Out-Default
  } else {
    $result = & py ($src + "/test.py")
    $exit = $?

    if (!$exit) {
      $result | Out-Default
    } else {
      & py ($src + "/main.py")
    }
  }
}