
import java.io.*;
import java.util.*;

public class Input {
    
    private Scanner input = new Scanner(System.in);
    
    public String readString(){
        String msg=input.next();
        return msg;
    }
    
    public int readInt(){
        boolean flag = false;
        int x = -1;
        while(!flag){
            try{
                x = Integer.parseInt(input.next());
                flag = true;
            }catch(NumberFormatException e){
                System.out.print("\f");
                System.out.println("Insira um numero (-1 para cancelar)");                
           }
        }
        return x;
    }
    
    public double readDouble(){
        boolean flag = false;
        double x = -1;
        while(!flag){
            try{
                x = Double.parseDouble(input.next());
                flag = true;
            }catch(NumberFormatException e){
                System.out.print("\f");
                System.out.println("Insira um numero (-1 para cancelar)");                
           }
        }
        return x;        
    }

    public float readFloat(){
        boolean flag = false;
        float x = -1;
        while (!flag){
            try{
                x = Float.parseFloat(input.next());
                flag = true;
            }catch (NumberFormatException e){
                System.out.println("\f");
                System.out.println("Insira um numero (-1 para cancelar)");
            }
        }
        return x;
    }
}
