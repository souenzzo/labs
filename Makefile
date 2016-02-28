

all: main.o
	gcc -o main.x main.o


%.o: %.c
	gcc -c $*.c


test:
	./main.x teste 01

