#include <stdlib.h>
#include <termios.h>
#include <ncurses.h>

int main(void){
    /*     Start curses mode    */
	initscr(); raw(); noecho();
    move(10,10);
	printw("Hello World !!!");	/* Print Hello World		  */
	refresh();			/* Print it on to the real screen */
	char c = getch();
    move(1,1);			/* Wait for user input */
    printw("%c",c);
    refresh();
    getch();
	endwin();			/* End curses mode		  */

    




	return 0;
}