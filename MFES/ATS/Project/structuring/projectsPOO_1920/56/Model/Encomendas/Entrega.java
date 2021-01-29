package Model.Encomendas;


import Model.Tipos.Empresa;
import Model.Tipos.ITipo;
import Model.Tipos.Tipo;
import Model.Tipos.Voluntario;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Objects;

public class Entrega extends Encomenda implements IEntrega, Serializable {
    private IEncomenda encomenda;
    private LocalTime horaEntrega;
    private ITipo transporte;
    private boolean entregue;
    private float distPercorrida;
    private LocalDate dataEntrega;
    private double precoTotal;

    public Entrega(){
        this.encomenda = new Encomenda();
        this.horaEntrega = LocalTime.now();
        this.transporte =null;/*
        this.transporte = new Empresa();*/
        this.entregue = false;
        this.distPercorrida = 0;
        this.dataEntrega = LocalDate.now();

    }

    public Entrega(IEncomenda encomenda , LocalTime horaEntrega,ITipo transporte,boolean entregue, float distPercorrida, LocalDate data){
        this.encomenda = encomenda;
        this.horaEntrega = horaEntrega;
        this.transporte = transporte;
        this.entregue = entregue;
        this.distPercorrida = distPercorrida;
        this.dataEntrega = data;
    }

    public Entrega(Entrega e){
        this.encomenda = e.getEncomenda();
        this.horaEntrega = e.getHoraEntrega();
        this.transporte = e.getTransporte();
        this.entregue = e.getEntregue();
        this.distPercorrida = e.getDistPercorrida();
        this.dataEntrega = e.getDataEntrega();
    }

    public LocalDate getDataEntrega() {
        return dataEntrega;
    }

    public void setDataEntrega(LocalDate dataEntrega) {
        this.dataEntrega = dataEntrega;
    }

    public float getDistPercorrida() {
        return distPercorrida;
    }

    public void setDistPercorrida(float distPercorrida) {
        this.distPercorrida = distPercorrida;
    }

    public boolean getEntregue() {
        return entregue;
    }

    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    public IEncomenda getEncomenda() {
        return this.encomenda;
    }

    public void setEncomenda(IEncomenda encomenda) {
        this.encomenda = encomenda;
    }

    public LocalTime getHoraEntrega() {
        return this.horaEntrega;
    }

    public void setHoraEntrega(LocalTime horaEntrega) {
        this.horaEntrega = horaEntrega;
    }

    public ITipo getTransporte() {
        return this.transporte;
    }

    public void setTransporte(ITipo transporte) {
        this.transporte = transporte;
    }

    public double getPrecoTotal() {
        return precoTotal;
    }

    public void setPrecoTotal(double precoTotal) {
        this.precoTotal = precoTotal;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Entrega)) return false;
        if (!super.equals(o)) return false;
        Entrega entrega = (Entrega) o;
        return getEntregue() == entrega.getEntregue() &&
                Float.compare(entrega.getDistPercorrida(), getDistPercorrida()) == 0 &&
                Objects.equals(getEncomenda(), entrega.getEncomenda()) &&
                Objects.equals(getHoraEntrega(), entrega.getHoraEntrega()) &&
                Objects.equals(getTransporte(), entrega.getTransporte()) &&
                Objects.equals(getDataEntrega(), entrega.getDataEntrega());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getEncomenda(), getHoraEntrega(), getTransporte(), getEntregue(), getDistPercorrida(), getDataEntrega());
    }

    @Override
    public String toString() {
        int classi = 0;
        if(transporte instanceof Voluntario) {
            classi = ((Voluntario) transporte).getVolunteer_rating();
        }
        else classi = ((Empresa)transporte).getClassificacao();
        return "Entrega:\n" +
                "Código: " + encomenda.getEncomendaID() +
                "\nHora da Entrega: " + horaEntrega +
                "\nTransporte: " + transporte.getNome() +
                "\nEntregue: " + entregue +
                "\nDistancia: " + distPercorrida +
                "\nData da Entrega: " + dataEntrega +
                "\nPreço Total: " + precoTotal +
                "\nClassificação: " + classi;
    }
}
