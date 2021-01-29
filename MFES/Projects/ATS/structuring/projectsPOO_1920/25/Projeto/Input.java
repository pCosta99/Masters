import static java.lang.System.out;
import static java.lang.System.in;
import java.util.Scanner;
import java.util.InputMismatchException;
import java.util.*;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.io.File;
public class Input implements Serializable{
    
    /**
     * Lê ficheiro em disco.
     */
    public static TrazAqui leFicheiro(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);
        TrazAqui t = (TrazAqui) ois.readObject();
        ois.close();
        return t;
    }
    
    /**
     * Lê ficheiro em texto.
     */
    public static List<String> lerFicheiroTexto(String nomeFicheiro) {
        Scanner read = null;
        List<String> linhas = new ArrayList<>();
        try {
            read = new Scanner(new File(nomeFicheiro));
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        while (read.hasNextLine())
            linhas.add(read.nextLine());   
        return linhas;
    }
    
    /**
     * Lê string.
     */
    public static String lerString() {
        Scanner input = new Scanner(in);
        boolean ok = false;
        String txt = "";
        while(!ok) {
            try {
                txt = input.nextLine();
                ok = true;
            }
            catch(InputMismatchException e)
            { out.println("Texto Invalido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return txt;
    }

    /**
     * Lê inteiro.
     */
    public static int lerInt() {
        Scanner input = new Scanner(in);
        boolean ok = false;
        int i = 0;
        while(!ok) {
            try {
                i = input.nextInt();
                ok = true;
            }
            catch(InputMismatchException e)
            { out.println("Inteiro Invalido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return i;
    }

    /**
     * Lê double.
     */
    public static double lerDouble() {
        Scanner input = new Scanner(in);
        boolean ok = false;
        double d = 0.0;
        while(!ok) {
            try {
                d = input.nextDouble();
                ok = true;
            }
            catch(InputMismatchException e)
            { out.println("Valor real Invalido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return d;
    }

    /**
     * Lê float.
     */
    public static float lerFloat() {
        Scanner input = new Scanner(in);
        boolean ok = false;
        float f = 0.0f;
        while(!ok) {
            try {
                f = input.nextFloat();
                ok = true;
            }
            catch(InputMismatchException e)
            { out.println("Valor real Invalido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return f;
    }

    /**
     * Lê boolean.
     */
    public static boolean lerBoolean() {
        Scanner input = new Scanner(in);
        boolean ok = false;
        boolean b = false;
        while(!ok) {
            try {
                b = input.nextBoolean();
                ok = true;
            }
            catch(InputMismatchException e)
            { out.println("Booleano Invalido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        return b;
    }
}