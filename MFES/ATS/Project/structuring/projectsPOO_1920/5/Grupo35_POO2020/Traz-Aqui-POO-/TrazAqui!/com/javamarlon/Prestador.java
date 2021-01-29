package com.javamarlon;

import java.util.ArrayList;
import java.util.HashMap;

public abstract class Prestador extends Entidade {

    private double raio;
    private boolean disponibilidade;
    protected HashMap<String, Encomenda> expedidas;
    protected ArrayList<Encomenda> entregues;

    public Prestador(String nome, GPS gps, double raio, String email, String password) {
        super(nome, gps, email, password);
        this.raio = raio;
        disponibilidade = true;
        expedidas = new HashMap<>();
        entregues = new ArrayList<>();
    }

    public double getRaio() {
        return raio;
    }

    public boolean isDisponivel() {
        return disponibilidade;
    }

    public boolean aoAlcance(Entidade entidade) {
        double raio = this.raio;
        GPS A = this.getGps();
        GPS B = entidade.getGps();
        double latA = A.getLatitude();
        double lonA = A.getLongitude();
        double latB = B.getLatitude();
        double lonB = B.getLongitude();

        double dist = Math.sqrt(Math.pow((latB - latA), 2) + Math.pow((lonB - lonA), 2));

        return ((raio - dist) >= 0);
    }


    public void concluirEntrega(String codigo) {
        if (expedidas.containsKey(codigo)) {
            Encomenda encomenda = expedidas.get(codigo);
            expedidas.remove(codigo);
            entregues.add(encomenda);
        }
    }

    public void receber(Encomenda encomenda) {
        expedidas.put(encomenda.getCodEncomenda(), encomenda);
    }

    @Override
    public String toString() {
        return super.toString() + "\n" +
                "Raio escolhido: " + raio + "\n" +
                "Disponibilidade : " + disponibilidade;
    }
}
