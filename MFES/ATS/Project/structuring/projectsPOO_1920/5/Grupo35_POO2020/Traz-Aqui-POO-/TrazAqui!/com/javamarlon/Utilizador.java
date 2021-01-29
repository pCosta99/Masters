package com.javamarlon;

import java.util.ArrayList;
import java.util.HashMap;

public class Utilizador extends Entidade {

    private ArrayList<Encomenda> pedidos; // Pedidos feitos pelo utilizador
    private HashMap<String, Encomenda> pendentes; // Encomendas propostas por transportadora
    private ArrayList<Encomenda> entregues; // Encomendas entregues. Para efeitos de histórico
    private HashMap<String, PropostaTransportadora> propostas;
    private HashMap<String, PropostaTransportadora> propostasAceites;

    public Utilizador(String nome, GPS gps, String email, String password) {
        super(nome, gps, email, password);
        this.pedidos = new ArrayList<>();
        this.entregues = new ArrayList<>();
        this.pendentes = new HashMap<>();
        this.propostas = new HashMap<>();
        this.propostasAceites = new HashMap<>();
    }

    public void fazerPedido(Encomenda encomenda) {
        this.pedidos.add(encomenda);
    }

    public void encomendaEntregue(Encomenda encomenda) {
        pedidos.remove(encomenda);
        entregues.add(encomenda);
    }

    public void printPedidos() {
        if (pedidos.isEmpty()) {
            System.out.println("\nNão existem pedidos.");
        } else {
            pedidos.forEach(System.out::println);
        }
    }

    public void printEntregues() {
        if (pedidos.isEmpty()) {
            System.out.println("\nNão existem pedidos.");
        } else {
            entregues.forEach(System.out::println);
        }
    }

    public HashMap<String, Encomenda> getPendentes() {
        return pendentes;
    }

    // pendentes.keySet().stream().map(key -> pendentes.get(key)).forEach(System.out::println);

    public void setPendente(Encomenda encomenda) {
        pendentes.put(encomenda.getCodEncomenda(), encomenda);
    }

    public void aceitarPendente(String cod) {
        pendentes.remove(cod);
    }

    public Encomenda getEncomendaPendente(String cod) {
        if (pendentes.containsKey(cod))
            return pendentes.get(cod);
        return null;
    }


    public void rejeitarPendente(String cod) {
        pendentes.remove(cod);
    }

    public ArrayList<Encomenda> getEntregues() {
        return entregues;
    }

    public HashMap<String, PropostaTransportadora> getPropostas() {
        return propostas;
    }

    public void addProposta(PropostaTransportadora proposta) {
        propostas.put(proposta.getEncomenda().getCodEncomenda(), proposta);
    }

    public void aceitarProposta(PropostaTransportadora proposta) {
        this.propostas.remove(proposta.getEncomenda().getCodEncomenda());
        this.propostasAceites.put(proposta.getEncomenda().getCodEncomenda(), proposta);
    }

    public void rejeitarProposta(String cod) {
        propostas.remove(cod);
    }
}
