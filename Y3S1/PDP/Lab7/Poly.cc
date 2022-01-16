#include <chrono>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <ctime>
#include <iostream>
#include <mpi.h>
#include <vector>

// https://stackoverflow.com/a/40808411/13253480
#if SIZE_MAX == UCHAR_MAX
#define MPI_USIZE MPI_UNSIGNED_CHAR
#elif SIZE_MAX == USHRT_MAX
#define MPI_USIZE MPI_UNSIGNED_SHORT
#elif SIZE_MAX == UINT_MAX
#define MPI_USIZE MPI_UNSIGNED
#elif SIZE_MAX == ULONG_MAX
#define MPI_USIZE MPI_UNSIGNED_LONG
#elif SIZE_MAX == ULLONG_MAX
#define MPI_USIZE MPI_UNSIGNED_LONG_LONG
#else
#error "Unsupported std::size_t size"
#endif

typedef std::size_t usize;

int randInt(int min, int max) { return min + rand() % (max - min + 1); }

class Poly: public std::vector<int> {
private:
  Poly(const std::vector<int>::const_iterator &start, const std::vector<int>::const_iterator &end): std::vector<int>(start, end) {}

public:
  Poly(): std::vector<int>() {}

  Poly(usize n): std::vector<int>(n) {}

  static Poly random(usize size, int min = -100, int max = 100) {
    Poly a{size};

    for (usize i = 0; i < a.size(); ++i)
      a[i] = randInt(min, max);

    return a;
  }

  usize degree() const {
    return size() - 1;
  }

  bool operator==(const Poly &b) const {
    if (size() != b.size())
      return false;

    for (usize i = 0; i < size(); ++i)
      if ((*this)[i] != b[i])
        return false;

    return true;
  }

  Poly operator+(const Poly &b) const {
    Poly c{std::max(size(), b.size())};

    for (usize i = 0; i < size(); ++i)
      c[i] += (*this)[i];

    for (usize i = 0; i < b.size(); ++i)
      c[i] += b[i];

    return c;
  }

  Poly& operator+=(const Poly& b) {
    return *this = *this + b;
  }

  Poly operator-() const {
    Poly c{size()};

    for (usize i = 0; i < size(); ++i)
      c[i] = -(*this)[i];

    return c;
  }

  Poly operator-(const Poly &b) {
    auto c = *this + (-b);

    auto last = c.size() - 1;
    usize i = 0;

    while (i <= last && c[last - i] == 0)
      ++i;

    if (i <= last)
      c.resize(last - i + 1);

    return c;
  }

  Poly operator<<(usize offset) const {
    Poly c{size() + offset};

    for (usize i = 0; i < size(); ++i)
      c[i + offset] = (*this)[i];

    return c;
  }

  Poly mulNaiveSeq(const Poly &b) const {
    Poly c{degree() + b.degree() + 1};

    for (usize i = 0; i < size(); ++i)
      for (usize j = 0; j < b.size(); ++j)
        c[i + j] += (*this)[i] * b[j];

    return c;
  }

  Poly fstHalf(usize mid) const {
    if (mid >= size())
      return *this;

    return Poly{begin(), begin() + mid};
  }

  Poly sndHalf(usize mid) const {
    if (mid >= size())
      return {0};

    return Poly{begin() + mid, end()};
  }

  Poly karatsubaSeq(const Poly &b, usize threshold = 10) const {
    if (degree() <= threshold || b.degree() <= threshold)
      return mulNaiveSeq(b);

    auto mid = std::max(size(), b.size()) / 2;

    auto aLow = fstHalf(mid);
    auto aHigh = sndHalf(mid);

    auto bLow = b.fstHalf(mid);
    auto bHigh = b.sndHalf(mid);

    auto z0 = aLow.karatsubaSeq(bLow);
    auto z1 = (aLow + aHigh).karatsubaSeq(bLow + bHigh);
    auto z2 = aHigh.karatsubaSeq(bHigh);

    return (z2 << (2 * mid)) + (z1 - z2 - z0 << mid) + z0;
  }
};

std::ostream &operator<<(std::ostream &os, const Poly &a) {
  os << '[';

  for (usize i = 0; i < a.size(); ++i)
    (i > 0 ? os << ", " : os) << a[i];

  return os << "]";
}

template <typename F> void timed(std::string label, F f) {
  auto start = std::chrono::high_resolution_clock::now();
  f();
  auto end = std::chrono::high_resolution_clock::now();

  auto elapsed =
      std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
          .count();
  std::cout << label << " took " << elapsed << "ms\n";
}

const int POLY_SIZE = 10'000;

void runSequential(usize rank) {
  if (rank != 0)
    return;

  auto a = Poly::random(POLY_SIZE, 1, 1000);
  auto b = Poly::random(POLY_SIZE, 1, 1000);
  Poly c1, c2;

  timed("Naive Sequential", [&] { c1 = a.mulNaiveSeq(b); });
  timed("Karatsuba Sequential", [&] { c2 = a.karatsubaSeq(b); });

  if (c1 != c2)
    std::cout << "Sequential: Mismatch\n";
}

void naiveWorker(usize nProc, usize rank) {
  usize polySize;
  MPI_Bcast(&polySize, 1, MPI_USIZE, 0, MPI_COMM_WORLD);

  Poly a{polySize}, b{polySize};
  MPI_Bcast(a.data(), polySize, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(b.data(), polySize, MPI_INT, 0, MPI_COMM_WORLD);

  auto elemsPerProc = polySize / (nProc - 1);
  auto extraElems = polySize % (nProc - 1);
  auto elemsMe = elemsPerProc + (rank <= extraElems);
  auto fstElem = (rank - 1) * elemsPerProc + std::min(rank - 1, extraElems);
  auto lastElem = fstElem + elemsMe;

  Poly c{a.degree() + b.degree() + 1};

  for (usize i = fstElem; i < lastElem; ++i)
    for (usize j = 0; j < b.size(); ++j)
      c[i + j] += a[i] * b[j];

  MPI_Ssend(c.data(), c.size(), MPI_INT, 0, 0, MPI_COMM_WORLD);
}

void naiveMaster(usize nProc) {
  auto a = Poly::random(POLY_SIZE, 1, 1000);
  auto b = Poly::random(POLY_SIZE, 1, 1000);

  usize polySize = a.size();
  MPI_Bcast(&polySize, 1, MPI_USIZE, 0, MPI_COMM_WORLD);
  MPI_Bcast(a.data(), polySize, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(b.data(), polySize, MPI_INT, 0, MPI_COMM_WORLD);

  Poly c{a.degree() + b.degree() + 1};

  for (usize i = 1; i < nProc; ++i) {
    Poly cTmp{c.size()};
    MPI_Recv(cTmp.data(), cTmp.size(), MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    for (usize j = 0; j < c.size(); ++j)
      c[j] += cTmp[j];
  }

  if (c != a.mulNaiveSeq(b))
    std::cout << "Naive MPI: Mismatch\n";
}

void runNaiveMPI(usize nProc, usize rank) {
  if (rank == 0)
    timed("Naive MPI", [&] { naiveMaster(nProc); });
  else
    naiveWorker(nProc, rank);
}

void karatsubaWorker(usize nProc, usize rank) {
  usize polySize;
  MPI_Bcast(&polySize, 1, MPI_USIZE, 0, MPI_COMM_WORLD);

  Poly a{polySize};
  MPI_Bcast(a.data(), polySize, MPI_INT, 0, MPI_COMM_WORLD);

  auto elemsPerProc = polySize / nProc;
  auto extraElems = polySize % nProc;
  auto elemsMe = elemsPerProc + (rank < extraElems);
  auto fstElem = rank * elemsPerProc + std::min(rank, extraElems);

  Poly b{elemsMe};
  MPI_Recv(b.data(), b.size(), MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  auto c = a.karatsubaSeq(b << fstElem);
  auto cSize = c.size();

  MPI_Ssend(&cSize, 1, MPI_USIZE, 0, 1, MPI_COMM_WORLD);
  MPI_Ssend(c.data(), c.size(), MPI_INT, 0, 2, MPI_COMM_WORLD);
}

void karatsubaMaster(usize nProc) {
  auto a = Poly::random(POLY_SIZE, 1, 1000);
  auto b = Poly::random(POLY_SIZE, 1, 1000);

  usize polySize = a.size();
  MPI_Bcast(&polySize, 1, MPI_USIZE, 0, MPI_COMM_WORLD);
  MPI_Bcast(a.data(), polySize, MPI_INT, 0, MPI_COMM_WORLD);

  auto elemsPerProc = polySize / nProc;
  auto extraElems = polySize % nProc;

  for (usize i = 1; i < nProc; ++i) {
    auto elemsMe = elemsPerProc + (i < extraElems);
    auto fstElem = i * elemsPerProc + std::min(i, extraElems);

    MPI_Ssend(&b[fstElem], elemsMe, MPI_INT, i, 0, MPI_COMM_WORLD);
  }

  auto b_ = b.fstHalf(elemsPerProc + (extraElems > 0));
  auto c = a.karatsubaSeq(b_);

  for (usize i = 1; i < nProc; ++i) {
    usize cSize;
    MPI_Recv(&cSize, 1, MPI_USIZE, i, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    Poly cTmp{cSize};
    MPI_Recv(cTmp.data(), cSize, MPI_INT, i, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    c += cTmp;
  }

  if (c != a.karatsubaSeq(b))
    std::cout << "Karatsuba MPI: Mismatch\n";
}

void runKaratsubaMPI(usize nProc, usize rank) {
  if (rank == 0)
    timed("Karatsuba MPI", [&] { karatsubaMaster(nProc); });
  else
    karatsubaWorker(nProc, rank);
}

int main(int, char **) {
  std::srand(std::time(0));
  std::cout << std::boolalpha;

  int nProcI, rankI;
  MPI_Init(0, 0);
  MPI_Comm_size(MPI_COMM_WORLD, &nProcI);
  MPI_Comm_rank(MPI_COMM_WORLD, &rankI);
  usize nProc = (usize)nProcI, rank = (usize)rankI;

  runSequential(rank);
  runNaiveMPI(nProc, rank);
  runKaratsubaMPI(nProc, rank);

  MPI_Finalize();
}
