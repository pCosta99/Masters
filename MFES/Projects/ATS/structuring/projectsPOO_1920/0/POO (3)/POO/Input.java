import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.InputMismatchException;
import java.util.Scanner;

public class Input implements Serializable {

    /**
     * Metodos de Classe
     */

    public void println(Object o) {
        System.out.println(o.toString());
    }

    public void print(Object o) {
        System.out.println(o.toString());
    }

    public String lerString() {

        Scanner input = new Scanner(System.in);
        boolean ok = false;
        String txt = "";
        while (!ok) {
            if (input.hasNext()) {
                try {
                    txt = input.nextLine();
                    ok = true;
                } catch (InputMismatchException e) {
                    println("Texto Inv�lido");
                    print("Novo valor: ");
                    input.nextLine();
                }
            }
        }
        input.close();
        return txt;
    }

    public int lerInt() {

        Scanner input = new Scanner(System.in);
        boolean ok = false;
        int i = 0;
        while (!ok) {
            if (input.hasNext()) {
                try {
                    i = input.nextInt();
                    ok = true;
                } catch (InputMismatchException e) {
                    println("Inteiro Inv�lido");
                    print("Novo valor: ");
                    input.nextLine();
                }
            }
        }
        input.close();
        return i;
    }

    public double lerDouble() {

        Scanner input = new Scanner(System.in);
        boolean ok = false;
        double d = 0.0;
        while (!ok) {
            try {
                d = input.nextDouble();
                ok = true;
            } catch (InputMismatchException e) {
                println("Valor real Inv�lido");
                print("Novo valor: ");
                input.nextLine();
            }
        }
        input.close();
        return d;
    }

    public float lerFloat() {

        Scanner input = new Scanner(System.in);
        boolean ok = false;
        float f = 0.0f;
        while (!ok) {
            try {
                f = input.nextFloat();
                ok = true;
            } catch (InputMismatchException e) {
                println("Valor Inv�lido");
                print("Novo valor: ");
                input.nextLine();
            }
        }
        input.close();
        return f;
    }

    public LocalDate lerData() {

        boolean flag = false;
        LocalDate localDate = LocalDate.now();
        do {
            try {
                println("Introduza a data da atividade(dd/MM/yyyy):");
                String data = lerString();
                SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
                Date date = sdf.parse(data);
                localDate = LocalDate.ofInstant(date.toInstant(), ZoneId.systemDefault());
                flag = true;
            } catch (ParseException e) {
                println("Formato de hora errada, tente outra vez.\n");
                flag = false;
            } catch (Exception e) {
                println("Formato de hora errada, tente outra vez.\n");
                flag = false;
            }
        } while (!flag);

        return localDate;
    }

   
    }

   

    

