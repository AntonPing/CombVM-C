.DEFAULT_GOAL := Norem
CCFLAG ?= -g -Wall -Wextra -Werror
CC := gcc

Norem: Norem.o NoremParse.o m
	$(CC) $(CCFLAG) -o $@ $^
.c.o:
	$(CC) $(CCFLAG) -c $<
run:
	./Norem
clean:
	rm -rf *.o main