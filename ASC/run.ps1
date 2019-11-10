param($src)

$tools = $env:ASM_TOOLS_PATH

$nasmPath = $tools + "nasm\"
$nasm = $tools + "nasm\nasm.exe"
$alink = $tools + "nasm\alink.exe"
$ollydbg = $tools + "ollydbg\ollydbg.exe"

$relSrc = ($PSScriptRoot + "\" + $src)
if (Test-Path $relSrc) {
  $src = $relSrc
}

$noExt = $src -replace "\.[^\.]+$"

$lst = ($noExt + ".lst")
$obj = ($noExt + ".obj")
$exe = ($noExt + ".exe")

if (Test-Path $src) {
  & $nasm -fobj $src -l $lst -I $nasmPath | Out-Default
}

if (Test-Path $obj) {
  & $alink -oPE -subsys console -entry start $obj | Out-Default
}

if (Test-Path $exe) {
  & $ollydbg $exe | Out-Default
}

if (Test-Path $lst) {
  Remove-Item $lst
}

if (Test-Path $obj) {
  Remove-Item $obj
}

if (Test-Path $exe) {
  Remove-Item $exe
}