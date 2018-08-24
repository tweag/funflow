#include <stdio.h>
#include <stdlib.h>



int collatz(int x)
{
  if (x % 2 == 0){
    return x / 2;
  } else {
    return (3 * x) + 1; 
  }
}



int main(int argc, char * argv[])
{
    FILE *fp;
    char buff[255];
    fp = fopen(argv[1], "r");
    fgets(buff, 255, fp);
    fclose(fp);

    int someInt;
    someInt = atoi( buff  );
    someInt = collatz (someInt) ;
    printf("%d\n", someInt);
/*
    FILE *fp2;
    fp2 = fopen(argv[2], "w+");
    char output[255];
    sprintf(output, "%d", someInt);
    fputs(output, fp2);
    fclose(fp2);
*/
    //printf("%i\n", someInt);
    return 0;
}



