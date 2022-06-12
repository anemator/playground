# References
# - https://gernotklingler.com/blog/gprof-valgrind-gperftools-evaluation-tools-application-level-cpu-profiling-linux/
CXXFLAGS = -O0
GPERFARGS = -DWITHGPERFTOOLS  -lprofiler -g

all: gperf gprof valgrind

# type: sampling
# low overhead (near native speed)
gperf:
	$(CXX) $(CXXFLAGS) main.cpp -o main-gperf $(GPERFARGS)
	./main-gperf || true
	google-pprof --pdf ./main-gperf main-gperf.prof > main-gperf.pdf

# type: instrumentation and sampling
# high overhead (from instrumentation)
# cannot profile multi-threaded apps or shared libraries
gprof:
	$(CXX) $(CXXFLAGS) -pg main.cpp -o main-gprof
	./main-gprof || true
	gprof ./main-gprof

# type: instrumentation (through VM via x86 -> RISC IR -- UCode)
# extremely slow but uniform so shouldn't affect results
valgrind: main
	valgrind --tool=callgrind ./main
	callgrind_annotate

clean:
	@rm -rvf callgrind.out.* gmon.out main main-*
