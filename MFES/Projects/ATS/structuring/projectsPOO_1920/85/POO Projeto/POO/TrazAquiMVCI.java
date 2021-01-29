import java.io.IOException;

public interface TrazAquiMVCI {

    //------------------------------ Ler e Escrever ------------------------------
    void read_TA();

    void carregaEstado() throws IOException, ClassNotFoundException;

    void guardaEstado() throws IOException;

    // ------------------------------- Menu ---------------------------------------------------
    void menu();
}
