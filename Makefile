

all: main.o
	gcc -o main.x main.o


%.o: %.c
	gcc -std=gnu11 -c $*.c


test:
	./main.x teste 01

