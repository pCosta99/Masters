package model;

import java.io.Serializable;

import static java.lang.Math.pow;
import static java.lang.Math.sqrt;

public class Viagem implements Serializable {

    private Coordenadas inicio;
    private Coordenadas fim;
    private String idEnc;
    
    public Viagem() {

        this.inicio = new Coordenadas();
        this.fim = new Coordenadas();
        this.idEnc = "";
    }
    
    public Viagem(Coordenadas init, Coordenadas end,String id) {

        this.inicio = init;
        this.fim = end;
    }
    
    public Viagem(Viagem novaViagem) {

        setInicio(novaViagem.getInicio());
        setFim(novaViagem.getFim());
    }
    

    public Coordenadas getInicio() {return this.inicio.clone();}
    public Coordenadas getFim() {return this.fim.clone();}

    public String getIdEnc() {
        return idEnc;
    }

    public void setIdEnc(String idEnc) {
        this.idEnc = idEnc;
    }

    public void setInicio(Coordenadas init) {this.inicio = init;}
    public void setFim(Coordenadas end) {this.fim = end;}
    
    public Viagem clone() {
        return new Viagem(this);
    }

    public double distanciaPercorrida(Coordenadas inicio, Coordenadas fim){
        double x = (pow((inicio.getLongitude()-fim.getLongitude()),2));
        double y = (pow((inicio.getLatitude()-fim.getLatitude()),2));
        return sqrt(x+y);
    }
}
