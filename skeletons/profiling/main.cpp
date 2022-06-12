#include <chrono>
#include <thread>

#ifdef WITHGPERFTOOLS
#include <gperftools/profiler.h>
#endif

void remove(unsigned long& total) {
  while (total > 0) {
    --total;
  }
  // need time to collect samples
  std::this_thread::sleep_for(std::chrono::milliseconds(500));
}

void add(unsigned long& total) {
  for (int i = 0; i < 1'000'000; ++i) {
    ++total;
  }
  // need time to collect samples
  std::this_thread::sleep_for(std::chrono::milliseconds(500));
}

void f2(unsigned long& total) {
# ifdef WITHGPERFTOOLS
    ProfilerStart("main-gperf.prof");
# endif
    for (int i = 0; i < 5; ++i) {
      add(total);
      remove(total);
    }
# ifdef WITHGPERFTOOLS
    ProfilerStop();
# endif
}

void f1(unsigned long& total) {
  f2(total);
}

int main() {
  auto total = 0ul;
  f1(total);
  return total;
}
