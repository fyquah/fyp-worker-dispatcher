all: fast.so

fast.so: fast.cpp
	g++ -I/usr/include/python2.7 -fPIC -g -shared -o $@ $^

clean:
	-rm libfast.so
