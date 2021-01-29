package view;

import model.Coordenadas;

import java.time.LocalDate;

public class RegistoUser {
    private final String id; //identificaçao de voluntario ou cliente
    private final String nome; //Nome de cliente ou taxista
    private final String email;//Endereço de email
    private final String password;
    private final String codPostal;//Codigo Postal; Necessario?
    private final String morada; // Necessario?
    private Coordenadas coor; //posicao exata
    private int raio; //raio de geográfico no qual se movimenta em kms
    private boolean special; //estatuto especial para encomendas medicas
    private boolean status;

    public RegistoUser(String id, String nome, String email, String password, String codPostal, String morada, Coordenadas coordMorada){
        this.id = id;
        this.nome = nome;
        this.email = email;
        this.password = password;
        this.codPostal = codPostal;
        this.morada = morada;
        this.coor= coordMorada;
    }

    public RegistoUser(String id, String nome, String email,
                      String password1, String codPostal, String morada, Coordenadas coor, int raio, boolean status, boolean special){


        this.id = id;
        this.nome = nome;
        this.email = email;
        this.password = password1;
        this.codPostal = codPostal;
        this.morada = morada;
        this.coor = coor;
        this.raio = raio;
        this.status = status;
        this.special = special;

    }

    public String getId() {
        return id;
    }

    public String getNome() {
        return nome;
    }

    public String getEmail() {
        return email;
    }

    public String getPassword() {
        return password;
    }

    public String getCodPostal() {
        return codPostal;
    }

    public String getMorada() {
        return morada;
    }



    public int getRaio() {
        return raio;
    }

    public void setRaio(int raio) {
        this.raio = raio;
    }

    public Coordenadas getCoor() {
        return coor;
    }

    public void setCoor(Coordenadas coor) {
        this.coor = coor;
    }

    public boolean isSpecial() {
        return special;
    }

    public boolean isStatus() {
        return status;
    }


}
