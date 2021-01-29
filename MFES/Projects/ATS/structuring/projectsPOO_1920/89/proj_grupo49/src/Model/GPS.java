package Model;

import java.io.Serializable;

/**
 * Write a description of class Model.GPS here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

/**
 * Classe que guarda a informação sobre a localicação de todas as entidades da aplicação
 */
public class GPS implements Serializable
{
    private double lat;
    private double lon;

    /**
     * Construtor sem parametros
     */
    public GPS()
    {
        this.lat = 0;
        this.lon = 0;
    }

    /**
     * Construtor parametrizado
     * @param x     double com latitude
     * @param y     double com longitude
     */
    public GPS (double x, double y)
    {
        this.lat = x;
        this.lon = y;
    }

    /**
     * Construtor por copia
     * @param u     GPS a copiar
     */
    public GPS (GPS u)
    {
        this.lat = u.getLat();
        this.lon = u.getLon();
    }

    /**
     * Get da variavel lat do objeto
     * @return      double com a latitude
     */
    public double getLat()
    {
        return this.lat;
    }

    /**
     * Get da variavel lon do objeto
     * @return      double com longitude
     */
    public double getLon()
    {
        return this.lon;
    }

    /**
     * Set da variavel lat do objeto
     * @param l     double com latitude
     */
    public void setLat (double l)
    {
        this.lat =l;
    }

    /**
     * Set da variavel lon do objeto
     * @param l     double com longitude
     */
    public void setLon (double l)
    {
        this.lon = l;
    }

    /**
     * Set de ambas as variaveis lat e lon
     * @param l     double com lat
     * @param lo    double com lon
     */
    public void setGPS (double l, double lo)
    {
        this.lat =l;
        this.lon = lo;
    }

    /**
     * Método que clona este objeto
     * @return      clone do objeto
     */
    public GPS clone()
    {
        return new GPS(this);
    }

    /**
     * Método equals do objeto
     * @param o     Objeto a comparar
     * @return      boolean
     */
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        GPS u = (GPS) o;
        return this.lat == u.getLat() && this.lon == u.getLon();
    }

    /**
     * Método que calcula a distancia entre 2 pontos no planeta
     * @param gps   GPS com coordenadas do segundo ponto
     * @return      double com distancia
     */
    public double distancia( GPS gps)
    {
        // Conversão de graus pra radianos das latitudes
        double firstLatToRad = Math.toRadians(this.lat);
        double secondLatToRad = Math.toRadians(gps.getLat());

        // Diferença das longitudes
        double deltaLongitudeInRad = Math.toRadians(gps.getLon()
                - this.lon);

        // Cálcula da distância entre os pontos
        return Math.acos(Math.cos(firstLatToRad) * Math.cos(secondLatToRad)
                * Math.cos(deltaLongitudeInRad) + Math.sin(firstLatToRad)
                * Math.sin(secondLatToRad))
                * 6378.137;
    }


        /**
     * Método que calcula a distancia entre 2 pontos x e y
     * @param gps   GPS com coordenadas do segundo ponto
     * @return      double com distancia
     */
    public double distanciaXY( GPS gps){
        return Math.sqrt(Math.pow(gps.getLat()- this.getLat(), 2) +Math.pow(gps.getLon()-this.getLon(), 2));
    }




    /**
     * Método que verifica se uma coordenada se encontra no alcance do raio de outra
     * @param gps   GPS com segunda coordenada
     * @param r     double com raio
     * @return      boolean
     */
    public boolean isNear(GPS gps, double r)
    {
        return (this.distanciaXY(gps)<=r);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("      Latitude = ").append(lat);
        sb.append("\n    Longitude = ").append(lon);
        return sb.toString();
    }
}