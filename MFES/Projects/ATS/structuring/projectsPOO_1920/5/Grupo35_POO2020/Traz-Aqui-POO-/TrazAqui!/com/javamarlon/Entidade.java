package com.javamarlon;

public abstract class Entidade {

    private String cod;
    private String nome;
    private GPS gps;
    private String email;
    private String password;

    public Entidade(String nome, GPS gps, String email, String password) {
        int index = email.indexOf('@');
        this.cod = email.substring(0, index);
        this.nome = nome;
        this.gps = gps;
        this.email = email;
        this.password = password;
    }

    public String getCod() {
        return cod;
    }

    public String getNome() {
        return nome;
    }

    public GPS getGps() {
        return gps;
    }

    public String getEmail() {
        return email;
    }

    public String getPassword() {
        return password;
    }

    public boolean checkLogin(String Password) {
        return password.equals(this.password);
    }

    public double distancia(Entidade entidade) {
        GPS A = this.getGps();
        GPS B = entidade.getGps();
        double latA = A.getLatitude();
        double lonA = A.getLongitude();
        double latB = B.getLatitude();
        double lonB = B.getLongitude();

        return Math.sqrt(Math.pow((latB - latA), 2) + Math.pow((lonB - lonA), 2));
    }

    @Override
    public String toString() {
        return "\nNome: " + nome + '\n' +
                "Username: " + cod + '\n' +
                "Email: " + email + '\n' +
                "Localização: " + gps;
    }
}
