param([switch] $nostrict, [string] $path)

$rel = ($PSScriptRoot + "/" + $path)
if (Test-Path $rel) {
  $path = $rel
}

if ($nostrict) {
  $strict = ""
} else {
  $strict = "--strict"
}

$result = & mypy $strict $path

if (!$?) {
  $result | Out-Default
} else {
  $test = ($path + "/test.py")

  if (Test-Path $test) {
    $result = & py $test

    if (!$?) {
      $result | Out-Default
    } else {
      $main = ($path + "/main.py")
      
      if (Test-Path $main) {
        & py $main
      }
    }
  }
}