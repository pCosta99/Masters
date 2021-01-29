package Auxiliares;

import java.io.Serializable;
import java.util.Objects;

public class GPS implements Serializable {

    /*
    Variaveis
     */

    private Double latitude;
    private Double longitude;

    /*
    Construtores
     */

    public GPS(Double latitude, Double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    public GPS() {
        this.latitude = 0.0;
        this.longitude = 0.0;
    }

    public GPS(GPS other){
        this.latitude = other.getLatitude();
        this.longitude = other.getLongitude();
    }

    /*
    Gets e Sets
     */

    public Double getLatitude() {
        return latitude;
    }

    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }

    public Double getLongitude() {
        return longitude;
    }

    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    /*
    Metodos
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GPS gps = (GPS) o;
        return Objects.equals(getLatitude(), gps.getLatitude()) &&
                Objects.equals(getLongitude(), gps.getLongitude());
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Latitude: ").append(this.latitude).append(" , ")
          .append("Longitude: ").append(this.longitude).append("\n");
        return sb.toString();
    }

    public GPS clone(){
        return new GPS(this);
    }



    // Calcula a distancia entre dois pontos
    public Double distanciaEntre (GPS b){

        double raioTerra = 6371;
        double lonA = Math.toRadians(this.getLongitude());
        double latA = Math.toRadians(this.getLatitude());
        double lonB = Math.toRadians(b.getLongitude());
        double latB = Math.toRadians(b.getLatitude());

        double dlon = lonB - lonA;
        double dlat = latB - latA;

        double A = Math.pow(Math.sin(dlat / 2), 2)
                + Math.cos(latA) * Math.cos(latB)
                * Math.pow(Math.sin(dlon / 2),2);
        double C = 2 * Math.asin(Math.sqrt(A));

        return (C*raioTerra); //30010 kms  //  dividir por 100 = 300,10 centenas de Km's
    }

    // Verifica se um ponto está dentro do raio de acao
    public Boolean estaDentro (Double distancia,Double raio){
        return distancia <= raio;
    }

    public String toStringGrava(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getLatitude()).append(",").append(this.getLongitude());

        return sb.toString();
    }

}
