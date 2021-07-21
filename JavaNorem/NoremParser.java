import java.util.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class NoremParser {
    public static void main(String[] args)
        throws IOException {
        // Enter data using BufferReader
        BufferedReader reader = new BufferedReader(
            new InputStreamReader(System.in));
 
        while(true) {
            System.out.print("> ");
            String name = reader.readLine();
            System.out.println(name);
        }
    }
}


interface Parsable<T> {
    public parse(Parser p) 
}
    


public class Parser {
    String text;
    int pos;
    Stack<int> stack;
    boolean pass;

    public Parser(String input){
        this.text = input;
        this.pos = 0;
        this.stack = new Stack<int>();
        this.pass = false;
    }


    public <T extends Parsable<T>> Optional<T> And() {
        if T.parse()


    }



}