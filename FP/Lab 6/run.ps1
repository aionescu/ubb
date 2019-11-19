& py test.py | Out-Default

if ($?) {
  & py main.py | Out-Default
}