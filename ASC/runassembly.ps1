# `runhaskell`-style command to run assembly files.
# Assembles, links, and runs the file in the debugger, then deletes all generated artifacts.

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

  $nasmResult = & $nasm -fobj $src -l $lst -I $nasmPath

  if (!$?) {
    $nasmResult | Out-Default
  } else {
    $alinkResult = & $alink -oPE -subsys console -entry start $obj
    $alinkCode = $?

    Remove-Item $lst
    Remove-Item $obj

    if (!$alinkCode) {
      $alinkResult | Out-Default
    } else {
      $(if ($dbg) { & $ollydbg $exe } else { & $exe }) | Out-Default
      Remove-Item $exe
    }
  }
}