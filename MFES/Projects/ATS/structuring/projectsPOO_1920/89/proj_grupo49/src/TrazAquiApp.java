import Controlador.ControladorGeral;
import Model.*;

import java.io.IOException;
import java.util.*;

public class TrazAquiApp {
    public static void main(String[] args) throws IOException, ClassNotFoundException {
        Lojas lojas = new Lojas();
        Transportadoras transportadoras = new Transportadoras();
        Voluntarios voluntarios = new Voluntarios();
        Utilizadores utilizadors = new Utilizadores();
        Map<String, Encomenda> encomendas = new TreeMap<>();
        Set<Produto> produtos;
        Modelo m = new Modelo(lojas, transportadoras, voluntarios, utilizadors, encomendas);

        ControladorGeral g = new ControladorGeral(m);
        g.run();
    }
}
