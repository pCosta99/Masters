
/**
 * Onde o programa inicia
 */
public class Main {

    public static void main(String[] args) {
        IModelo m = new SistemaTrazAqui();
        IVista v = new Vista();
        IControlador c = new Controlador(v, m);
        c.run();
    }
}
