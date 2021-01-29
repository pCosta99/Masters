
/*
 *  Análise e Teste de Software
 *  João Saraiva
 *  2016-2017
 */


#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "rapl.h"

#define RUNTIME


int main (int argc, char **argv)
{ char command[500],res[500];
  int  ntimes = 1;
  int  core = 0;
  int  i=0;

#ifdef RUNTIME
  clock_t begin, end;
  double time_spent;

  struct timeval tvb,tva;

#endif

  FILE * fp;

  // printf("Program to be executed: %d",argc);
 /* strcpy( command, "./" ); */
 /* strcat(command,argv[1]); */
 strcpy(command, argv[1]);
 printf("Program to be executed: %s\n",argv[1]);

  /* strcpy(command, "./" ); */
  /* strcat(command,argv[1]); */

  ntimes = atoi (argv[2]);

  strcpy(res,argv[3]);
  strcat(res,".J");
  printf("Command: %s  %d-times res: %s\n",command,ntimes,res);


  printf("\n\n RUNNING THE PARAMETRIZED PROGRAM:  %s\n\n\n",command);
  fflush(stdout);

  fp = fopen(res,"w");
  rapl_init(core);

  fprintf(fp,"Program, Package , Core(s) , GPU , DRAM? , Time (sec) \n");


  for (i = 0 ; i < ntimes ; i++)
    {
        fprintf(fp,"%s , ",argv[1]);
        rapl_before(fp,core);

#ifdef RUNTIME
        begin = clock();
	gettimeofday(&tvb, 0);
#endif

        system(command);

#ifdef RUNTIME
	end = clock();
	gettimeofday(&tva, 0);
	/* time_spent = (double)(end - begin) / CLOCKS_PER_SEC; */
	time_spent = ((tva.tv_sec-tvb.tv_sec)*1000000 + tva.tv_usec-tvb.tv_usec)/1000000.0;
#endif

	rapl_after(fp,core);

#ifdef RUNTIME
	fprintf(fp," %G \n",time_spent);
#endif
    }

  printf("\n\n END of PARAMETRIZED PROGRAM: \n");

  fclose(fp);
  fflush(stdout);

  return 0;
}



