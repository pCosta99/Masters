/**
 * Classe que representa uma coordenada
 */
package Model;

import java.io.Serializable;

public class Coordenadas implements Serializable {

    private double latitude;
    private double longitude;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public Coordenadas() {
        this(0, 0);
    }

    /**
     * construtor classe coordenada
     *
     * @param latitude  latitude
     * @param longitude longitude
     */
    public Coordenadas(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * construtor classe coordenada
     *
     * @param c coordenada
     */
    public Coordenadas(Coordenadas c) {
        this.longitude = c.getLongitude();
        this.latitude = c.getLatitude();
    }

    //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * devolve a latitude
     *
     * @return latitude
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * altera a latitude
     *
     * @param latitude latitude
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * devolve a longitude
     *
     * @return longitude
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * altera a longitude
     *
     * @param longitude longitude
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    //--------------------------------------------------------------Equals, toString e clone--------------------------------------------------------------------------\\

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Coordenadas that = (Coordenadas) o;
        return Double.compare(that.latitude, latitude) == 0 &&
                Double.compare(that.longitude, longitude) == 0;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("Coordenadas{");
        sb.append("latitude=").append(latitude);
        sb.append(", longitude=").append(longitude);
        sb.append('}');
        return sb.toString();
    }

    public Coordenadas clone() {
        return new Coordenadas(this);
    }

    //--------------------------------------------------------------Outros métodos--------------------------------------------------------------------------\\

    /**
     * calcula distancia entre 2 coordenada
     *
     * @param c coordenada
     * @return  double distancia
     */
    public double distancia(Coordenadas c) {
        return Math.sqrt((this.latitude - c.getLatitude()) * (this.latitude - c.getLatitude()) +
                        (this.longitude - c.getLongitude()) * (this.longitude - c.getLongitude()));
    }
}

