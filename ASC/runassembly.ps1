param([switch] $dbg, [string] $src)

$relSrc = ($PSScriptRoot + "/" + $src)
if (Test-Path $relSrc) {
  $src = $relSrc
}

if (Test-Path $src) {
  $tools = $env:ASM_TOOLS_PATH

  $nasmPath = $tools + "nasm/"
  $nasm = $tools + "nasm/nasm.exe"
  $alink = $tools + "nasm/alink.exe"
  $ollydbg = $tools + "ollydbg/ollydbg.exe"

  $noExt = $src -replace "\.[^\.]+$"

  $lst = ($noExt + ".lst")
  $obj = ($noExt + ".obj")
  $exe = ($noExt + ".exe")

  $result = & $nasm -fobj $src -l $lst -I $nasmPath

  if (!$?) {
    $result | Out-Default
  } else {
    $result = & $alink -oPE -subsys console -entry start $obj
    $exit = $?

    Remove-Item $lst
    Remove-Item $obj

    if (!$exit) {
      $result | Out-Default
    } else {
      $(if ($dbg) { & $ollydbg $exe } else { & $exe }) | Out-Default
      Remove-Item $exe
    }
  }
}