package TrazAqui;

import java.io.Serializable;
import java.util.Objects;

import static java.lang.Math.pow;
import static java.lang.Math.sqrt;
/**
 * Classe que representa um Ponto TrazAqui!
 */
public class Ponto implements Serializable {
    /**
     * Valor da latitude no par
     */
    private double latitude;
    /**
     * Valor da longitude no par
     */
    private double longitude;
    /**
     * Método para obter a latitude
     * @return latitude float
     */
    public double getLatitude() {
        return latitude;
    }
    /**
     * Método para obter a longitude
     * @return longitude float
     */
    public double getLongitude() {
        return longitude;
    }
    /**
     * Método para dar set da latitude
     * @param latitude float
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }
    /**
     * Método para dar set da longitude
     * @param longitude float
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }
    /**
     * Construtor parametrizado de um objeto Ponto
     * @param latitude Double que representa a latitude do objeto Ponto
     * @param longitude Double que representa a longitude do objeto Ponto
     */
    public Ponto(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }
    /**
     * Construtor vazio do objeto Ponto
     */
    public Ponto() {
        this.latitude = 0.0;
        this.longitude = 0.0;
    }

    /**
     * Construtor de cópia da Classe Ponto
     * @param other Ponto a copiar
     */
    public Ponto(Ponto other){
        this.latitude = other.latitude;
        this.longitude = other.longitude;
    }
    /**
     * Método para verificar se um certo objeto é igual a Ponto
     * @return boolean
     * @param o Object
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Ponto ponto = (Ponto) o;
        return Double.compare(ponto.latitude, latitude) == 0 &&
                Double.compare(ponto.longitude, longitude) == 0;
    }


    /**
     * Método que cria uma String de um objeto em concreto (Neste caso de Ponto)
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Latitude: ").append(this.latitude).append("\n");
        sb.append("Longitude: ").append(this.longitude).append("\n");
        return sb.toString();
    }
    /**
     * Método clone
     */
    @Override
    public Ponto clone(){
        return new Ponto(this);
    }
    /**
     * Método que calcula a distancia de um certo ponto a este ponto devolvendo um double
     * @param p Ponto
     * @return distancia double
     */
    public double distancia(Ponto p){
        return sqrt(pow((this.latitude+p.getLatitude()),2) + pow(this.longitude+p.getLongitude(),2));
    }

}

