package MVC.Controller;

import static java.lang.System.in;

import java.util.InputMismatchException;
import java.util.Scanner;

public class Input {

/**
 * Métodos de Classe
 */

    private static Scanner input = new Scanner(in);
    
    public static String lerString(String x) {
        System.out.println(x);
        Input.input = new Scanner(System.in);
        boolean ok = false;
        String txt = "";
        while(!ok) {
            txt = Input.input.nextLine();
            ok = true;
        }
        return txt;
    }

    public static String lerString() {
        return Input.lerString("");
    }

    public static int lerInt(){
    return Input.lerInt("Inteiro: ");
    }

    public static int lerInt(String x) {
    Input.input = new Scanner(System.in);
    System.out.println(x);
    boolean ok = false; 
    int i = 0;
    while(!ok) {
        try {
            
        } catch (InputMismatchException e) {
            //TODO: handle exception
        }
        i = Input.input.nextInt();
        ok = true;
    }
    return i;
    } 
    
    public static double lerDouble() {
    Input.input = new Scanner(System.in);
    boolean ok = false; 
    double d = 0.0; 
    while(!ok) {
        d = input.nextDouble();
        ok = true;
    }
    return d;
    }  
    
    public static boolean lerBoolean() {
    Input.input = new Scanner(System.in);
    boolean ok = false; 
    boolean b = false; 
    while(!ok) {
        b = Input.input.nextBoolean();
        ok = true;
    }
    return b;
    } 


}