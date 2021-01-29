import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Map;

public interface VoluntarioI extends TransporteI{
    double getRaio();
    void setRaio(double raio);
    boolean isDisponivel();
    void setDisponivel(boolean disponivel);
    double getClassificacao();
    void setClassificacao(double classificacao);
    List<Transporte_EncomendaI> getHistEntregas();
    void setHe(List<Transporte_EncomendaI> tes);
    Voluntario clone();
    String toString();
    boolean equals(Object obj);
    void disponivel(boolean d);
    boolean podeIrBuscar(double xL, double yL, double xU, double yU);
    double distLoja(double xL, double yL);
    void atualizaClassificacao(double cl);
    void addEncomenda(Transporte_EncomendaI te);
    double distTotal(double xL, double yL, double xU, double yU);
    void entregaEncomendaVl(Transporte_EncomendaI te, double kmsFeitos, char b, int fila, LocalDateTime inicio);
    LocalTime tempoTransporte(int fila, double kmsFeitos);
    String infVoluntario();
    void leTA(String cod, String[] p);
    void atualizaEnc(Map<String, EncomendaI> le);
}
