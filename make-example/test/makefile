source-files: main.cpp factorial.cpp hello.cpp functions.h

hello: main.o factorial.o hello.o functions.h
    g++ main.o factorial.o hello.o -o hello

main.o: main.cpp functions.h
    g++ -c main.cpp

factorial.o: factorial.cpp functions.h
    g++ -c factorial.cpp

hello.o: hello.cpp functions.h
    g++ -c hello.cpp
