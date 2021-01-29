package com.javamarlon;

public class PropostaTransportadora {

    private Encomenda encomenda;
    private double custo;
    private Transportadora transportadora;

    public PropostaTransportadora(Encomenda encomenda, double custo, Transportadora transportadora) {
        this.encomenda = encomenda;
        this.custo = custo;
        this.transportadora = transportadora;
    }

    public Encomenda getEncomenda() {
        return encomenda;
    }

    public double getCusto() {
        return custo;
    }

    public Transportadora getTransportadora() {
        return transportadora;
    }

    @Override
    public String toString() {
        return "\n" + encomenda + "\nCusto: " + custo + "\nTransportadora: " + transportadora.getNome();
    }
}
