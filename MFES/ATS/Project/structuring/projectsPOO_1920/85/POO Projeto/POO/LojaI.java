import java.util.List;

public interface LojaI extends Traz_Aqui_ComumI{
    List<String> getFe();
    void setFe(List<String> les);
    String toString();
    boolean equals(Object obj);
    Loja clone();
    EncomendaI addPeso(EncomendaI e, double peso);
    int filaEspera();
    void addEncomenda(String e);
    void encomendaLevantada(String e);
    void leTA(String cod, String[] p);
    //void atualizaEnc(Map<String, EncomendaI> le);
}
