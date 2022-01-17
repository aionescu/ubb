> "$1"
> "$2"

for ((i = 3; i <= $#; ++i)); do
  for file in $(ls ${!i}); do
    if [ -f $file ]; then
      cat $file | grep -o "[13579]" >> $1
      cat $file | grep -o "[02468]" >> $2
    fi
  done
done
