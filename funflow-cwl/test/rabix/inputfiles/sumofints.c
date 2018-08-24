#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main(int argc, char * argv[])
{
    FILE *fp;
    char filename[255];

    long long sum = 0;
    char currIntStr[255];
    int currint;

    for (int i = 0; i < (argc - 1); i++){
      strcpy(filename, argv[i+1]);
      fp = fopen(filename,"r");
      fgets(currIntStr, 255, fp);
      currint = atoi(currIntStr);
      sum += currint;
      fclose(fp);
    }
    printf("%lld\n", sum);
    return 0;
}



