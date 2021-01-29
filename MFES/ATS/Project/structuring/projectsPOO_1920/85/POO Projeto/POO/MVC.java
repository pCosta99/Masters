import java.io.IOException;

public class MVC {
    public static void main(String[] args) throws IOException, ClassNotFoundException {
        TrazAquiMVCI mvc = new TrazAquiMVC();

        mvc.carregaEstado();
        //mvc.read_TA(); Apenas se usa caso falhe ao ler em binario
        mvc.menu();
        mvc.guardaEstado();
    }
}
