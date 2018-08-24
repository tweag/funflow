#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main(int argc, char * argv[])
{
    FILE *fp;
    char buff[255];
    char filename[255];
    char postfix[100];
    int numfiles = atoi( argv[1] );
    int randomint;
    char fileval[255];

    for (int i = 0; i < numfiles; i++){
      strcpy(filename, "file");
      //filename = "file";
      sprintf(postfix, "%d", i);
      strcat(filename, postfix);
      fp = fopen(filename,"w+");
      randomint = rand();
      sprintf(fileval, "%d", randomint);
      fputs(fileval, fp);
      fclose(fp);
    }
    return 0;
}



