import java.io.IOException;

public class Main {

    public static void main(String[] args) throws IOException, ClassNotFoundException {
        Model model = new Model();
        View view = new View();
        Controller controller = new Controller();

        controller.setModel(model);
        controller.setView(view);
        controller.iniciarPaginaInicial();
    }
}



