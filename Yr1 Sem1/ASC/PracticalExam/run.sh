wine ~/Tools/ASMTools/nasm/nasm.exe -fobj $1.asm

if [ -f $1.obj ]; then
  wine ~/Tools/ASMTools/nasm/alink.exe -oPE -subsys console -entry start $1.obj

  if [ -f $1.exe ]; then
    echo "Running:"
    wine $1.exe

    rm $1.exe
  fi

  rm $1.obj
fi