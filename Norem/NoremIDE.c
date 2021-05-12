#include "Norem.h"
#include   <termios.h>

#define oops(s, x) { perror(s); exit(x); }

void fun_set(struct termios *info, char set);//设置回显位,设置缓冲
void fun_backspace();//实现退格功能

int main()
{
    int c;
    int i, j;

    struct termios info;
    fun_set( &info, 0 );//关掉回显位,关掉缓冲
    
    char str[30];//保存输出的字符
    int p = 0;//当前位置
    int len = 0;//总长度

    while( ( c=getchar() ) != EOF )
    {
        if( c == 'a' && p ) {
            putchar('我');
        
        } else if( isalnum(c) ) {
            //isalnum()函数:如果c是一个数字或字母返回非0值，否则为0
            //user input a letter or a number
            //1.将当前位置之后的值依次后移
            j = ++len;
            while( j-- > p )
                str[j] = str[j-1];
            str[p] = c;
            j = len - p - 1;//光标要移动的距离
            //2.从当前位置开始重新输出数组
            while( p < len )
                putchar(str[p++]);
            //3.将光标移动到之前的位置
            while( j-- > 0 && p-- )
                putchar('\b');
        }
        else if( c == 0x7f && p )
        {
            //退格键(user input a backspace)
            j = len - p;//光标要移动的距离
            //1.将当前位置之后的值依次前移
            putchar('\b');
            while( p < len )
            {
                str[p-1] = str[p];
                putchar(str[p]);
                p++;
            }
            //2.将最后一个元素删除
            putchar(' ');
            putchar('\b');
            len--;
            p--;
            //3.将光标移动到之前的位置
            while( j-- > 0 && p-- )
                putchar('\b');
        }
        else if ( c == '[' && p )
        {
            //使用'['进行左移光标
            putchar('\b');
            p--;
        }
        else if ( c == ']' && p < len )
        {
            //使用']'进行右移光标
            //将当前位置的值再输出一遍
            putchar(str[p++]);
        }
        else if ( c == 0x7e && len )
        {
            //删除键(Del):删除整行(user input delete)
            //1.从光标处移动到结尾
            while( ++p <= len )
                putchar(' ');
            //2.从结尾往前依次退格
            while( --p )
                fun_backspace();
            //3.len置0
            len = 0;
        }
    }

    fun_set( &info, 1 );//打开回显位,打开缓冲
}

void fun_set(struct termios *info, char set)
{
    if ( tcgetattr(0, info) == -1 )          /* get attribs   */
        oops("tcgettattr", 1);
    /*set为1,打开回显位,打开缓冲;set为0,关掉回显位,关掉缓冲*/
    if( set )
    {
        (*info).c_lflag |= ECHO;    /* turn on bit   */
        (*info).c_lflag &= ICANON;  /* turn on bit   */
    }
    else
    {
        (*info).c_lflag &= ~ECHO;   /* turn off bit   */
        (*info).c_lflag &= ~ICANON; /* turn off bit   */
    }
    if ( tcsetattr(0, TCSANOW, info) == -1 ) /* set attribs    */
        oops("tcsetattr",2);
}

void fun_backspace()
{
    putchar('\b');
    putchar(' ');
    putchar('\b');
}