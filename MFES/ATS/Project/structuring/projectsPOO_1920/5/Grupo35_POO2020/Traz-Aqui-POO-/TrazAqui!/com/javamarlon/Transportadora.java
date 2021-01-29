package com.javamarlon;

import java.util.HashMap;

// TODO: Implementar a noção de rota. Recolher até N encomendas numa loja. Entregar de forma ordenada de acordo com a distancia.
//  Se sobrar capacidade ou se encomendas=N e entregar uma delas é possível recolher outra encomenda na loja mais próxima.
public class Transportadora extends Prestador {

    private String NIF;
    private double taxa;
    private int capacidade;
    private HashMap<String, Encomenda> pendentesAceitacao;
    private HashMap<String, PropostaTransportadora> propostasAceites;


    public Transportadora(String nome, GPS gps, double raio, String NIF, double taxa, int capacidade, String email, String password) {
        super(nome, gps, raio, email, password);
        this.NIF = NIF;
        this.taxa = taxa;
        this.capacidade = capacidade;
        this.pendentesAceitacao = new HashMap<>();
        this.propostasAceites = new HashMap<>();
    }

    public String getNIF() {
        return NIF;
    }

    public double getTaxa() {
        return taxa;
    }

    public int getCapacidade() {
        return capacidade;
    }

    public boolean disponivel() {
        return expedidas.size() < capacidade;
    }

    public HashMap<String, Encomenda> getExpedidas() {
        return expedidas;
    }

    public void aceitarEncomenda(Encomenda encomenda) {
        pendentesAceitacao.put(encomenda.getCodEncomenda(), encomenda);
    }

    public void utilizadorAceita(String cod) {
        Encomenda encomenda = pendentesAceitacao.get(cod);
        pendentesAceitacao.remove(cod);
        expedidas.put(cod, encomenda);
    }

    public HashMap<String, Encomenda> getPendentesAceitacao() {
        return pendentesAceitacao;
    }


    @Override
    public String toString() {
        return super.toString() + "\n" +
                "NIF: " + NIF + '\n' +
                "Taxa de entrega: " + taxa + "\n" +
                "Capacidade: " + capacidade;
    }

    public void utilizadorRejeita(String cod) {
        pendentesAceitacao.remove(cod);
    }

    public void propostaAceite(PropostaTransportadora proposta) {
        this.propostasAceites.put(proposta.getEncomenda().getCodEncomenda(), proposta);
    }
}
