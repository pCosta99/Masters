import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Classe responsável pelo Input do Utilizador
 */
public class Input {
    private final Vista v;

    public Input() {
        this.v = new Vista();
    }

    public int lerInt() {
        Scanner input = new Scanner(System.in);
        boolean ok = false;
        int i = -1;
        while (!ok) {
            try {
                i = input.nextInt();
                ok = true;
            } catch (InputMismatchException e) {
                v.showMessage("Tem de introduzir um inteiro\n");
                v.showMessage("> ");
                input.nextLine();
            }
        }
        return i;
    }

    public String lerString() {
        Scanner input = new Scanner(System.in);
        boolean ok = false;
        String txt = "";
        while (!ok) {
            try {
                txt = input.nextLine();
                ok = true;
            } catch (InputMismatchException e) {
                v.showMessage("Texto inválido\n");
                v.showMessage("Introduza outro > ");
                input.nextLine();
            }
        }
        return txt;
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
                v.showMessage("Valor real inválido, introduza outro > ");
                input.nextLine();
            }
        }
        return d;
    }
}