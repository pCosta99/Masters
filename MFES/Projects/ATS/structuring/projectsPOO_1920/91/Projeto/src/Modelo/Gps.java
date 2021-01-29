/**
 * Classe que representa as cordenadas
 */
package Modelo;

import java.io.Serializable;

import static java.lang.StrictMath.sqrt;

//CONSTRUTORES

public class Gps implements Serializable {
    private double x;
    private double y;

    /**
     * Contrutor vazio
     */
    public Gps(){
        this.x = 0;
        this.y = 0;
    }

    /**
     * Construtor parametrizado
     * @param x x
     * @param y y
     */
    public Gps(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Construtor cópia
     * @param outro gps
     */
    public Gps (Gps outro){
        this.x = outro.x;
        this.y = outro.y;
    }

    //GETTERS E SETTERS

    /**
     * Método que devolve a cordenada y
     * @return y
     */
    public double getY() {
        return this.y;
    }

    /**
     * Método que modifica a cordenada y
     * @param y y
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * Método que devolve a cordenada x
     * @return x
     */
    public double getX() {
        return this.x;
    }

    /**
     * Método que modifica a cordenada x
     * @param x x
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Método que faz um clone de um gps
     * @return gps
     */
    @Override
    public Gps clone() {
        return new Gps(this);
    }

    @Override
    public boolean equals(Object obj) {
        if(this == obj) return true;
        if (this.getClass() != obj.getClass() || obj==null) return false;
        Gps a = (Gps) obj;
        return this.x == a.getX() && this.y == a.getY();
    }

    @Override
    public String toString() {
        return "Coordenadas do GPS: x-> " + this.x + " y-> " + this.y;
    }

    /**
     * Método que devolve a distancia entre 2 cordenadas
     * @param gps
     * @return distancia
     */
    public double distanciaCoordenadas (Gps gps){
        return sqrt(((this.getX() - gps.getX()) * ((this.getX()) - gps.getX())) +
                (((this.getY()) - gps.getY()) * ((this.getY()) - gps.getY())));
    }
}
