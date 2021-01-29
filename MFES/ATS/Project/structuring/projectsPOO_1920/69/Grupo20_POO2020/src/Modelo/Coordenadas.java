package Modelo;

import java.io.Serializable;

/**
 * Classe que implemente uma Coordenada
 */
public class Coordenadas implements Serializable {
    private double latitude;
    private double longitude;

    // --------------------------- Constructor -------------------------

    /**
     * Construtor por omissão
     */
    public Coordenadas(){
        this.latitude = 0;
        this.longitude = 0;
    }

    /**
     * Construtor parametrizado
     * @param latitude              Latitude
     * @param longitude             Longitude
     */
    public Coordenadas(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Construtor por cópia
     * @param c             Coordenadas
     */
    public Coordenadas(Coordenadas c){
        this.latitude = c.getLatitude();
        this.longitude = c.getLongitude();
    }

    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve a latitude
     * @return double
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Define latitude
     * @param latitude      Latitude
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Devolve longitude
     * @return double
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Define longitude
     * @param longitude         Longitude
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    // --------------------------- Auxiliaries -------------------------

    /**
     * Devolve uma cópia da instancia
     * @return Coordenadas
     */
    public Coordenadas clone (){
        return new Coordenadas(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o     Objeto a comparar
     * @return boolean
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Coordenadas)) return false;
        Coordenadas that = (Coordenadas) o;
        return Double.compare(that.getLatitude(), getLatitude()) == 0 &&
                Double.compare(that.getLongitude(), getLongitude()) == 0;
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string.
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder(" Modelo.Coordenadas {");
        sb.append("latitude = ").append(latitude);
        sb.append(", longitude = ").append(longitude);
        sb.append('}');
        return sb.toString();
    }


    /**
     * Função que calcula a distancia entre coordenadas (this e b)
     * @param b            Coordenada b
     * @return double valor da distancia.
     */
    public double distance(Coordenadas b) {
        return Math.sqrt((Math.pow(b.getLatitude()-this.latitude,2))+(Math.pow(b.getLongitude()-this.longitude,2)));
    }
}
