package Model;

import java.io.Serializable;
import java.util.Random;
import java.lang.Math.*;

/**
 * Classe que define coordenadas GPS
 */
public class GPS implements Serializable {

    private Double lon;
    private Double lat;

    /**
     * Construtor da classe
     *
     * @param x A longitude
     * @param y A latitude
     */
    public GPS(Double x, Double y) {
        this.lon = x;
        this.lat = y;
    }

    /**
     * Construtor da classe
     */
    public GPS() {
        lon = 0.0;
        lat = 0.0;
    }

    /**
     * Construtor da classe
     *
     * @param gps O GPS do qual se extrai a informação
     */
    public GPS(GPS gps) {
        lon = gps.lon;
        lat = gps.lat;
    }

    /**
     * Define a longitude
     *
     * @param x A longitude a ser definida
     */
    public void setLon(Double x) {
        this.lon = x;
    }

    /**
     * Define a latitude
     *
     * @param y A latitude a ser definida
     */
    public void setLat(Double y) {
        this.lat = y;
    }

    /**
     * Indica a longitude
     *
     * @return A longitude
     */
    public Double getLon() {
        return lon;
    }

    /**
     * Indica a latitude
     *
     * @return A latitude
     */
    public Double getLat() {
        return lat;
    }

    /**
     * Cria um clone do GPS
     *
     * @return O clone do GPS
     */
    public GPS clone() {
        return new GPS(this);
    }

    /**
     * Gera coordenadas GPS aleatórias
     */
    public void randomGPS() {
        Random rand = new Random();
        this.lon = rand.nextDouble()*200-100;
        this.lat = rand.nextDouble()*200-100;
    }

    /**
     * Verifica se um dado objeto é igual a um GPS
     *
     * @param o O objeto com o qual se pretende comparar
     * @return True caso sejam iguais e false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof GPS)) return false;
        GPS gps = (GPS) o;
        return lon.equals(gps.lon) &&
                lat.equals(gps.lat);
    }

    /**
     * Converte um GPS numa String
     *
     * @return A String correspondente
     */
    public String toString() {
        return "GPS: (" + lon +
                ", " + lat +
                ')';
    }

    /**
     * Indica a distância em kms entre dois pontos em coordenadas GPS
     *
     * @return A distância em kms
     */
    public double distGPSKms(GPS gps) {

        double x1, x2, y1, y2;

        x1 = this.getLon();
        y1 = this.getLat();

        x2 = gps.getLon();
        y2 = gps.getLat();


        //Calcula a distância entre os dois pontos em kms
        return Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))/10;

    }

}
