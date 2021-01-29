
/*
 *  Análise e Teste de Software
 *  João Saraiva 
 *  2016-2017
 */

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "F_stub.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "rapl.h"

#define RUNTIME


/*
hs_init() at the start
hs_exit() at the end
run code to measure between rapl_before and rapl_after

*/

int main (int argc, char **argv) 
{ char command[500],res[500];
  int  ntimes = 1;
  int  core = 0;
  int  i=0;

  hs_init(&argc, &argv);

#ifdef RUNTIME
  clock_t begin, end;
  double time_spent;

  struct timeval tvb,tva;
  
#endif
  
  FILE * fp;

  // printf("Program to be executed: %d",argc);
  //strcpy( command, "./" );
 
  strcpy(res,"output");
  strcat(res,".J");
  
  fflush(stdout);
  
  fp = fopen(res,"w");
  rapl_init(core);

  fprintf(fp,"Program, Package , Core(s) , GPU , DRAM? , Time (sec) \n");

  int v;
  printf("Enter a number: \n");scanf("%d",&v);

  
  for (i = 0 ; i < ntimes ; i++)
    {
        fprintf(fp,"%s , ",argv[1]);
        rapl_before(fp,core);
      
#ifdef RUNTIME
        begin = clock();
	gettimeofday(&tvb, 0);
#endif
	
    //code to measure

  printf("\n\n fib(%d): %lld \n\n",v,fib_hs(v));
  //   printf("\n\n fibe(%d): %lld \n\n",v,fibe(v));


  //  printf("\n\n fib(%d): %lld \n\n",45,fib(45));
  //  printf("\n\n fibE(%d): %lld \n\n",45,fibe(45));
//    printf("Fibonacci efficient(%d): %d\n", v, fibe(v));


#ifdef RUNTIME
	end = clock();
	gettimeofday(&tva, 0);
	//	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	time_spent = (tva.tv_sec-tvb.tv_sec)*1000000 + tva.tv_usec-tvb.tv_usec;
#endif

	rapl_after(fp,core);

#ifdef RUNTIME	
	fprintf(fp," %G \n",time_spent);
#endif	
    }
    
  printf("\n\n Done: \n");

  fclose(fp);
  fflush(stdout);

  hs_exit();
  return 0;
}



