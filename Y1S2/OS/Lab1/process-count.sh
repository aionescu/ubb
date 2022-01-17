ps aux | cut -f 1 -d " " | sort | uniq -c | sort -g -r
