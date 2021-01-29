import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

public interface TransporteI extends Traz_Aqui_ComumI{
    double getRaio();
    void setRaio(double raio);
    boolean isDisponivel();
    void setDisponivel(boolean disponivel);
    double getClassificacao();
    void setClassificacao(double classificacao);
    List<Transporte_EncomendaI> getHistEntregas();
    void setHe(List<Transporte_EncomendaI> tes);
    String toString();
    boolean equals(Object o);
    Transporte clone();
    void disponivel(boolean d);
    boolean podeIrBuscar(double xL, double yL, double xU, double yU);
    double distLoja(double xL, double yL);
    void atualizaClassificacao(double cl);
    void addEncomenda(Transporte_EncomendaI te);
    double distTotal(double xL, double yL, double xU, double yU);
    LocalDateTime somaHoras(LocalDateTime inicio, LocalTime time);
}
