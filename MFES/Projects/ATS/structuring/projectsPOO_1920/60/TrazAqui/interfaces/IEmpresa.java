package interfaces;

import java.time.LocalDateTime;

public interface IEmpresa extends IEntregas {
    String getNif();
    double getTaxa();
    void setTaxa(double tax);
    boolean apenasUmaEncomenda();
    int getNumEnc();
    double custoEntrega(ISistema s, String e);
    IEmpresa clone();
    double faturacao(ISistema s, LocalDateTime inicio, LocalDateTime fim);
}
