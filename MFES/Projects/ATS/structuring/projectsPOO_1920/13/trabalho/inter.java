import java.io.*;
import java.nio.file.*;
import java.util.*;
public class inter
{
    public inter(){
        Scanner input = new Scanner(System.in);
        String s="1 -> LOGIN"+ "\n" +"2 -> REFGISTAR";
        System.out.println(s);
        List<String> arr = new ArrayList<String>(2); 
        arr.add("2"); 
        arr.add("1");
        String op=input.nextLine();
        while(!arr.contains(op)){
            s="input invalido tente outra vez";
            System.out.println(s);
            op=input.nextLine();
        }
        
        if(op.equals("1")){
            new login();
        }
        if(op.equals("2")){
            new registo();
        }
        
    }
}