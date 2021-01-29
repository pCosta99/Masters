/*
 *  Análise e Teste de Software
 *  2016-2017
 */


#include "rapl.h"
#include "fib.h"



long long fib(long long n)
{ if   (n <= 1 ) return 1;
  else return (fib(n-1) + fib(n-2)); 
}



long long fibe (long long n)
{ RES r = fib2(n);
  return r.nM2;
}


RES fib2(long long n)
{ RES r; 
  if (n <= 1 ) 
    { r.nM1 = 1;
      r.nM2 = 1;
    }
  else
    {  long long a;
       r = fib2(n-1);
       a = r.nM1;
       r.nM1 = r.nM2;
       r.nM2 = r.nM2 + a;
    }
  return r;
}




int main (int argc, char **argv) 
{
  int core = 0;

  int v = atoi (argv[1]);

  //int v = 30;
  long long res;

  res = fib(v);
  printf("\n\n fib(%d): %lld \n\n",v,res);
  
  fflush(stdout);
    
  return 0;
}



