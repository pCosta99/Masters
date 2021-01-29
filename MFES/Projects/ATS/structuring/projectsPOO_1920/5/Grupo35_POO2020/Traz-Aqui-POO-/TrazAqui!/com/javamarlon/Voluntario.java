package com.javamarlon;

public class Voluntario extends Prestador {


    public Voluntario(String nome, GPS gps, double raio, String email, String password) {
        super(nome, gps, raio, email, password);
    }

    public boolean disponivel() {
        return (expedidas.isEmpty());
    }
}
