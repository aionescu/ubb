g++ -std=c++17 -Wall -Wextra -pedantic --coverage Domain.cc Repo.cc InMemoryRepo.cc FileRepo.cc Services.cc UI.cc $1
gcov $1
./a.out
lcov -c --directory . --output-file Coverage.info --no-external
genhtml --output-directory coverage-cc-html Coverage.info
rm -r *.gcov *.gcno *.gcda *.info a.out