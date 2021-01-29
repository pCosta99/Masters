package Model.Encomendas;

import Model.Tipos.ITipo;

import java.time.LocalDate;
import java.time.LocalTime;

public interface IEntrega {

    LocalDate getDataEntrega();
    void setDataEntrega(LocalDate dataEntrega);
    float getDistPercorrida();
    void setDistPercorrida(float distPercorrida);
    boolean getEntregue();
    void setEntregue(boolean entregue);
    LocalTime getHoraEntrega();
    void setHoraEntrega(LocalTime horaEntrega);
    ITipo getTransporte();
    void setTransporte(ITipo transporte);
    IEncomenda getEncomenda();
    void setEncomenda(IEncomenda encomenda);
    double getPrecoTotal();
    void setPrecoTotal(double precoTotal);
}
