& mypy --strict $PSScriptRoot | Out-Default

if ($?) {
  & py test.py | Out-Default

  if ($?) {
    & py main.py | Out-Default
  }
}