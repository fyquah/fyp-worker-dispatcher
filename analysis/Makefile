all: fast.so fast_analysis.so

fast_analysis.so: fast_analysis.cpp
	g++ -O3 -I/usr/include/python2.7 -fPIC -g -shared -o $@ $^ -ltbb

fast.so: fast.cpp
	g++ -O3 -I/usr/include/python2.7 -fPIC -g -shared -o $@ $^

clean:
	-rm fast.so
