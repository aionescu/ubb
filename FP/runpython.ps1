param([string] $path)

$rel = ($PSScriptRoot + "/" + $path)
if (Test-Path $rel) {
  $path = $rel
}

$result = & mypy $path

if (!$?) {
  $result | Out-Default
} else {
  $test = ($path + "/test.py")

  if (Test-Path $test) {
    $result = & py $test

    if (!$?) {
      $result | Out-Default
    }
  }

  & py ($path + "/main.py")
}