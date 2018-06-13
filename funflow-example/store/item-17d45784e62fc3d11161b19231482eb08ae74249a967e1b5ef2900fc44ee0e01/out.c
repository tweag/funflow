#include <stdio.h>
#include <stdlib.h>
int times2(int n);
int square(int n);
int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  int r = times2(n) + square(n);
  printf("%d\n", r);
}