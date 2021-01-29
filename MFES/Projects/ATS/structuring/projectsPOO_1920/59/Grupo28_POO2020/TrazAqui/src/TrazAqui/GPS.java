package TrazAqui;

import java.io.Serializable;
import java.lang.Math;

public class GPS implements Serializable {
    /**
     * Variaveis de instancia
     */
    private double latitude;
    private double longitude;

    /**
     * Construtor vazio
     */
    public GPS () {
        this.latitude = 0;
        this.longitude = 0;
    }

    /**
     * Construtor parametrizado
     * @param latitude double
     * @param longitude double
     */
    public GPS(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Construtor por copia 
     * @param a GPS
     */
    public GPS (GPS a) {
        this.longitude = a.getLongitude();
        this.latitude = a.getLatitude();
    }

    /**
     * Retorna a latitude
     * @return double
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Define a latitude 
     * @param latitude double
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Retorna a longitude 
     * @return double
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Define a longitude 
     * @param longitude double
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Compara o objeto recebido com o que chama
     * @param o Object
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GPS gps = (GPS) o;
        return Double.compare(gps.latitude, latitude) == 0 &&
                Double.compare(gps.longitude, longitude) == 0;
    }

    /**
     * Retorna os dados o do GPS em formato string
     * @return String
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("GPS {");
        sb.append("latitude = ").append(latitude);
        sb.append(", longitude = ").append(longitude);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Calcula a distancia entre duas posicoes
     * @param a GPS
     * @return double
     */
    public double distancia(GPS a) {
        double dist;
        if((this.latitude == a.latitude) && (this.longitude == a.longitude)) dist=0;
        else {
            double theta;
            theta = this.longitude - a.getLongitude();
            dist = Math.sin(Math.toRadians(this.latitude)) * Math.sin(Math.toRadians(a.latitude)) + Math.cos(Math.toRadians(this.latitude)) * Math.cos(Math.toRadians(a.latitude)) * Math.cos(Math.toRadians(theta));
            dist = Math.toDegrees(Math.acos(dist));
            dist = dist * 60 * 1.1515 * 1.609344;
        }
        return dist/1000;
    }
}
