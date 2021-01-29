import java.time.LocalDateTime;
import java.util.List;

public interface UtilizadorI extends Traz_Aqui_ComumI{
    String toString();
    boolean equals(Object obj);
    Utilizador clone();
    String informacaoTransporte(List<Transporte_EncomendaI> tes, LocalDateTime inicio, LocalDateTime fim);
    void leTA(String cod, String[] p);
}
