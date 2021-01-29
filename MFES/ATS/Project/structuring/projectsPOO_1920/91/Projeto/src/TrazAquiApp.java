import Controlador.Controlador;
import Controlador.IControlador;

public class TrazAquiApp {
    public static void main(String[] args) {

        IControlador controlador = new Controlador();
        controlador.run();
    }
}
