package TrazAqui;

import java.io.IOException;

/**
 * A função com  a qual é iniciado o programa.
 */
public class Main {
    public static void main(String[] args) {
        Menu m = new Menu();
        try {
            m.run();
        } catch (IOException | LojaInexistenteException e) {
            e.printStackTrace();
        }
    }
}