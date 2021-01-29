

Análise e Teste de Software - 2020
Universidade do Minho


-- To generate the main program that monitors energy consumption
-- of programs that receive as arguments:

make


-- (an execultable called main is produced)

-- program main recieves as argument the program to be executed and
-- the number of times such program will be run.

-- To test the main program we can use the fib.c program.

--  First, compile the program fib.c (that computes the fibonnaci number
-- passed as argument)

gcc -O2 -o fib fib.c


-- we can now run it with a given number:

./fib 22


 fib(22): 28657 


-- To monitor energy of the fibonnacci program (executing it one time)

--  first we need to give access to the register where energy is defined:

sudo modprobe msr


-- and now we can run the main program

sudo ./main "fib 22" 1


-- a file "fib 22.J" will be produced with the energy/execution times!

