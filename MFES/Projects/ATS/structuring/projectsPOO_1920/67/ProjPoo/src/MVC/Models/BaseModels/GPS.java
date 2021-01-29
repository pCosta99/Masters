package MVC.Models.BaseModels;

import java.io.Serializable;

/**
 * Contém as coordenadas de um user
 * em termos de latitude e longitude.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */
public class GPS implements Serializable {
    private double x;
    private double y;

    /**
     * Construtor GPS por defeito.
     */
    public GPS(){
        this.x = 0;
        this.y = 0;
    }

    /**
     * Construtor GPS parametrizado.
     * @param a Coordenada X.
     * @param b Coordenada Y.
     */
    public GPS(double a, double b){
        this.x = a;
        this.y = b;
    }

    /**
     * Construtor GPS por Cópia.
     * @param g GPS a copiar.
     */
    public GPS(GPS g){
        this.x = g.getX();
        this.y = g.getY();
    }

    /**
     * Método que retorna a Coordenada X do GPS.
     * @return Coordenada X.
     */
    public double getX(){
        return this.x;
    }

    /**
     * Método que retorna a Coordenada Y do GPS.
     * @return Coordenada Y.
     */
    public double getY(){
        return this.y;
    }

    /**
     * Método que define a Coordenda X do GPS.
     * @param a Coordenada X.
     */
    public void setX(double a){
        this.x = a;
    }

    /**
     * Método que define a Coordenada Y do GPS.
     * @param a Coordenada Y.
     */
    public void setY(double a){
        this.y = a;
    }

    /**
     * Método que define as Coordenadas do GPS.
     * @param a Coordenada X.
     * @param b Coordenda Y.
     */
    public void setXY(double a,double b){
        this.x = a;
        this.y = b;
    }

    /**
     * Método equals.
     * @param o Object a comparar.
     * @return True caso sejam iguais, false caso contrário.
     */
    public boolean equals(Object o){
        if (this == o) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        GPS g = (GPS) o;
        return (this.getX()==g.getX() && this.getY()==g.getY());
    }

    /**
     * Método que devolve a Distância entre dois GPS's.
     * @param g GPS.
     * @return Distância resultante.
     */
    public double distancia(GPS g){
        return Math.sqrt(Math.pow(g.x-this.x,2)+Math.pow(g.y-this.y,2));
    }

    /*public double distanciaKm(GPS g){
        double raioTerra = 6371; // em Km
        GPS g1 = this.toKms();
        GPS g2 = g.toKms();
        double deltaLat = (g1.getX()-g2.getX());
        double deltaLon = (g1.getY()-g2.getY());
        double a = ((Math.sin(deltaLat/2) * Math.sin(deltaLat/2))+ Math.cos(g1.getX()))
                *Math.cos(g2.getX())*(Math.sin(deltaLon/2) * Math.sin(deltaLon/2));
        double c = 2*Math.atan(Math.sqrt(a)/Math.sqrt(1-a));
        return raioTerra*c;
    }*/

    /**
     * Método toString.
     * @return String que contém os dados do GPS.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("GPS: (").append(this.getX())
        .append(",").append(this.getY()).append(")");
        return sb.toString();
    }

    /**
     * Método Clone.
     * @return GPS clonado.
     */
    public GPS clone(){
        return new GPS(this);
    }
}