g++ -std=c++17 -Wall -Wextra -pedantic --coverage $1

gcov $1
./a.out

lcov -c --directory . --output-file Coverage.info
genhtml Coverage.info --output-directory coverage-html

rm -r *.gcov *.gcno *.gcda *.info a.out coverage-html