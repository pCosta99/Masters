import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Map;

public interface Empresa_TransportadoraI extends TransporteI{
    int getNif();
    void setNif(int nif);
    double getTaxa();
    void setTaxa(double taxa);
    double getKmsFeitos();
    void setKmsFeitos(double kmsFeitos);
    int getLotacaoEnc();
    void setLotacaoEnc(int lotacaoEnc);
    int getOcupacaoEnc();
    void setOcupacaoEnc(int ocupacaoEnc);
    Empresa_Transportadora clone();
    String toString();
    boolean equals(Object obj);

    double precoTransporte(double xL, double yL, double xU, double yU, double peso);
    void entregaEncomendaTr(Transporte_EncomendaI te, double kmsFeitos, char b, int fila, LocalDateTime inicio);
    LocalTime tempoTransporte(int fila, double kmsFeitos);
    double totalFaturado(LocalDateTime inicio, LocalDateTime fim);
    String infTransportadora();

    void leTA(String cod, String[] p);
    void atualizaEnc(Map<String, EncomendaI> le);

}
