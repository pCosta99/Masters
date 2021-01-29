package model;

import java.io.Serializable;

import static java.lang.Math.*;

public class Coordenadas implements Serializable {

    private double longitude ;
    private double latitude ;

    // Construtores usuais
    public Coordenadas ( double x , double y ) {
        setLongitude(x);
        setLatitude(y);
    }

    public Coordenadas() {
        this (0.0 , 0.0) ;
    }
    public Coordenadas ( Coordenadas p ) {
        longitude = p.getLongitude () ;
        latitude = p.getLatitude() ;
    }


    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public void setLongitude(double longitude){
        this.longitude = longitude;
    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public String toString(){
        return new String ( this.longitude + "º" + "," + this.latitude + "º");

    }

    public boolean equals(Object cor){
        if (cor == this) return true;

        if (cor == null || cor.getClass() != this.getClass()) return false;

        Coordenadas cord = (Coordenadas) cor;

        return this.getLatitude() == cord.getLatitude()
                && this.getLongitude() == cord.getLongitude();
    }


    public Coordenadas clone(){
        return new Coordenadas(this);
    }

    /*
    Tipo parse de uma Coordenada
     */

    public Coordenadas stringToCoordenadas(String s){
        Coordenadas c = new Coordenadas();
        String[] partes = s.split(",");
        c.setLatitude(Double.parseDouble(partes[0]));
        c.setLongitude(Double.parseDouble(partes[1]));
        return c;


    }

    /*
    Distancia entre duas coordenadas
     */
    public double distancia(Coordenadas c){
        double x = (pow((c.getLongitude()-this.getLongitude()),2));
        double y = (pow((c.getLatitude()-this.getLatitude()),2));
        return sqrt(x+y);
    }

}
