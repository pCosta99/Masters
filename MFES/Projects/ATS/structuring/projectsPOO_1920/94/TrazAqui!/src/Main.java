import Controller.Controller;
import Models.FileLoaders;
import Models.Sistema;

/**
 * Classe Main da aplicacao
 */
public class Main {

    public static final Sistema s = new Sistema();
    public static FileLoaders f = new FileLoaders();
    public static Controller control = new Controller();

    public static void main(String[] args) {

        f.loadLogs(s);

        control.app(s);
    }
}
