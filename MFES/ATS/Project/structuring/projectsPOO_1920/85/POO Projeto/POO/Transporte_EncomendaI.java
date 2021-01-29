import java.time.LocalDateTime;

public interface Transporte_EncomendaI extends EncomendaI{
    LocalDateTime getInicioTransporte();
    void setInicioTransporte(LocalDateTime inicioTransporte);
    LocalDateTime getFimTransporte();
    void setFimTransporte(LocalDateTime fimTransporte);
    double getCustoTransporte();
    void setCustoTransporte(double custoTransporte);
    Transporte_Encomenda clone();
    String toString();
    boolean equals(Object o);
}
