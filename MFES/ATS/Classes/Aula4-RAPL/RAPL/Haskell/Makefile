fib-rapl: F.hs rapl.c main.c
	stack ghc -c -O F.hs
	stack ghc --make -no-hs-main -optc-O main.c rapl.c F -o fib-rapl
	chmod 777 fib-rapl

clean:
	rm *.o *.hi fib-rapl
