import java.io.Serializable;
import java.lang.Math;


/**
 * Classe usada para guardar a latitude e longitude de um determinado user,
 * assim como fazer as operacoes sobre GPS
 */

public class GPS implements Serializable
{
    private double latitude;
    private double longitude;

    public GPS()
    {
        this.latitude = 0;
        this.longitude = 0;
    }

    public GPS(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    public GPS (GPS g) {
        this.latitude = g.getLatitude();
        this.longitude = g.getLongitude();
    }

    public double getLatitude() {
        return this.latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return this.longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }


    public GPS clone (){
        return new GPS ( this );
    }


    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof GPS)) {
            return false;
        }
        GPS gPS = (GPS) o;
        return latitude == gPS.latitude && longitude == gPS.longitude;
    }


    public String toString() {
        
        StringBuilder sb = new StringBuilder();

        sb.append("latitude = ").append(this.getLatitude() + ",").append("longitude = ").append(this.getLongitude());

        return sb.toString();   
    }


    /**
     * Funcao auxiliar da dist para colocar um dado valor em radianos
    */

    private double toRadianos (double valor){
        return valor*Math.PI/180;
    }

    /**
     * Para calcular a distancia entre 2 pontos com latitutes e longitudes usamos a
     * chamada "Lei de Haversine"
    */

    public double dist( GPS gps2) 
    {

    double R     = 6378.137;  //Raio da Terra em km
    double dLat  = toRadianos( gps2.getLatitude() - this.latitude);
    double dLong = toRadianos( gps2.getLongitude() - this.longitude );

    double a = Math.sin(dLat/2) * Math.sin(dLat/2) + Math.cos(toRadianos(this.latitude)) * Math.cos(toRadianos(gps2.getLatitude())) * Math.sin(dLong/2) * Math.sin(dLong/2);
    double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    double d = R * c;

    return d;
    }

    /**
     * Nesta funcao vemos se o nosso objeto (Gps) esta dentro do raio de outro Gps 
     * passado por parametro chamado ponto
    */

    public boolean dentroRaio (GPS ponto , double raio){

        if (this.dist(ponto) <= raio) return true; // se a distancia entre as duas coordenadas for menor que o raio

        else return false;
    }



}
