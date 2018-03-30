all: fast.so

fast.so: fast.cpp
	g++ -O3 -I/usr/include/python2.7 -fPIC -g -shared -o $@ $^

clean:
	-rm fast.so
