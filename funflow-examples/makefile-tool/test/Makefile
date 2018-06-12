source-files: main.cpp factorial.cpp hello.cpp functions.h

hello: main.o factorial.o hello.o functions.h
    gcc main.o factorial.o hello.o -o hello

main.o: main.cpp functions.h
    gcc -c main.cpp

factorial.o: factorial.cpp functions.h
    gcc -c factorial.cpp

hello.o: hello.cpp functions.h
    gcc -c hello.cpp
