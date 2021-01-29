package Geral;

import java.io.Serializable;

public class GPS implements Serializable {
    private double latitude;
    private double longitude;

    /**
     * Construtor por omissão.
     */
    public GPS() {
        this.latitude = 0;
        this.longitude = 0;
    }

    /**
     * Cosntrutor por cópia.
     * @param g GPS a ser copiado.
     */
    public GPS(GPS g) {
        this.latitude = g.getLatitude();
        this.longitude = g.getLongitude();
    }

    /**
     * Construtor por parâmetros.
     * @param latitude Latitude.
     * @param longitude Longitude.
     */
    public GPS(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Método que devolve a latitude de uma dada localização.
     * @return Latitude.
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Método que define a latitute de uma dada localização.
     * @param latitude Latitude.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Método que devolve a longitude de uma dada localização.
     * @return Longitude.
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Método que define a longitude de uma dada localização.
     * @param longitude Longitude.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Método que transforma numa String a informação sobre uma dada localização.
     * @return String coma  informação da localização.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.latitude).append(",").append(this.longitude);
        return sb.toString();
    }

    /**
     * Método que cria uma cópia de uma dada localização.
     * @return Cópia da localização.
     */
    public GPS clone(){
        return new GPS(this);
    }

    /**
     * Método que averigua se um dado objeto é igual a uma dada localização.
     * @param o Objeto a ser comparado.
     * @return Resultado da comparação.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GPS gps = (GPS) o;
        return Double.compare(gps.getLatitude(), getLatitude()) == 0 &&
                Double.compare(gps.getLongitude(), getLongitude()) == 0;
    }

    /**
     *  Distância entre 2 localizações GPS
     * @param that 2º ponto GPS
     * @return Distância
     */
    public double distanceTo(GPS that){
        double lat1 = Math.toRadians(this.getLatitude());
        double lat2 = Math.toRadians(that.getLatitude());
        double lon1 = Math.toRadians(that.getLatitude()-this.getLatitude());
        double lon2 = Math.toRadians(that.getLongitude()-this.getLongitude());

        double a = Math.sin(lon1/2) * Math.sin(lon1/2) +
                Math.cos(lat1) * Math.cos(lat2) *
                        Math.sin(lon2/2) * Math.sin(lon2/2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));

        return 6371 * c;

    }
}