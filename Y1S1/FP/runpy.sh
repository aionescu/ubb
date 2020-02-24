#!/usr/bin/sh

if [ $1 = "--no-strict" ]; then
  strict=""
  path=$2
else
  strict="--strict"
  path=$1
fi

case $path in
  /*) path=$path;;
  *) path=$PWD/$path;;
esac

python3 -m mypy $strict $path

if [ $? -eq 0 ]; then
  test=$path/test.py

  if [ -f $test ]; then
    python3 $test

    if [ $? -eq 0 ]; then
      main=$path/main.py

      if [ -f $main ]; then
        python3 $main
      fi
    fi
  fi
fi