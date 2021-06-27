make:
	clear
	gcc -no-pie -pthread Norem.h \
	NoremFunc.c NoremEval.c NoremMem.c \
	NoremParse.c NoremTest.c NoremDict.c NoremShow.c
	./a.out

ide:
	clear
	gcc NoremIDE.c
	./a.out