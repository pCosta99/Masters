import java.io.Serializable;
import java.util.List;

public interface EncomendaI extends Serializable {
    String getCodEncomenda();
    void setCodEncomenda(String codEncomenda);
    String getCodUtilizador();
    void setCodUtilizador(String codUtilizador);
    String getCodLoja();
    void setCodLoja(String codLoja);
    double getPeso();
    void setPeso(double peso);
    List<Linha_EncomendaI> getLe();
    void setLe(List<Linha_EncomendaI> les);
    Encomenda clone();
    String toString();
    boolean equals(Object obj);
    void criarEncomenda(String codEncomenda, String codUtilizador, String codLoja, List<String> list);
    void encomendaRandom(String codEnc, String codUtilizador, String codLoja, boolean medico);
    void leEnc(String cod, String[] p);
}
