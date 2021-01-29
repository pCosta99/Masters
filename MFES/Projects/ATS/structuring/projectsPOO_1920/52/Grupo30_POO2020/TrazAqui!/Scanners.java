import java.io.Serializable;
import java.util.Date;
import java.util.Scanner;
import static java.lang.System.out;

public class Scanners implements Serializable
    {
        public static Scanner stdin = new Scanner(System.in);

        public static int leituraInt(String s){

            System.out.println(s);
            int aux = stdin.nextInt();
            stdin.nextLine();
            return aux;
        }

        public static float leituraFloat(String s){

            System.out.println(s);
            float aux = stdin.nextFloat();
            stdin.nextLine();
            return aux;

        }

        public  static  String leituraString(String s){

            System.out.println(s);
            String aux = stdin.nextLine();
            return aux;
        }
        
        public  static  double leituraDouble(String s){

            System.out.println(s);
            double aux = stdin.nextDouble();
            stdin.nextLine();
            return aux ;
        }

}