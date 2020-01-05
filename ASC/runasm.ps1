param([switch] $dbg, [Parameter(ValueFromRemainingArguments)] [string[]] $files)

function AbsolutePath {
  param([string] $path)
  
  $rel = ($PSScriptRoot + "/" + $path)

  if (Test-Path $rel) {
    return $rel
  } else {
    return $path
  }
}

function ToObj {
  param([string] $path)

  $noExt = $path -replace "\.[^\.]+$"

  return ($noExt + ".obj")
}

function ToExe {
  param([string] $path)

  $noExt = $path -replace "\.[^\.]+$"

  return ($noExt + ".exe")
}

$files = @($files | ForEach-Object { AbsolutePath $_ })

$tools = $env:ASM_TOOLS_PATH

$nasm = $tools + "nasm/nasm.exe"
$alink = $tools + "nasm/alink.exe"
$ollydbg = $tools + "ollydbg/ollydbg.exe"

$objs = @($files | ForEach-Object { ToObj $_ })

$result = $files | ForEach-Object { & $nasm -fobj $_ }

if (!$?) {
  $result | Out-Default
} else {
  $result = & $alink -oPE -subsys console -entry start @objs
  $exit = $?

  $objs | ForEach-Object { Remove-Item $_ }

  if (!$exit) {
    $result | Out-Default
  } else {
    $exe = ToExe ($files[0])
    
    if ($dbg) {
      & $ollydbg $exe | Out-Default
    } else {
      & ("./" + $exe) | Out-Default
    }
      
    Remove-Item $exe
  }
}