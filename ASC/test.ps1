param([switch] $dbg, [Parameter(ValueFromRemainingArguments)] [string[]] $src)

foreach ($arg in $src) {
  Write-Host $arg
}