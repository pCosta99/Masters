import java.io.*;

/**
 * Classe Localização :: Coordenadas GPS de cada Utilizador
 * Coordenadas caracterizadas por latitude e longitude.
 */
public class Localizacao implements Serializable
{
  private double lat;
  private double lon;
  
  public Localizacao() {
      this.lat = 0;
      this.lon = 0;
  }
    
  public Localizacao(double lat, double lon) {
      this.lat = lat;
      this.lon = lon;
  }
    
  public Localizacao(Localizacao coord) {
      this.lat = coord.getLat();
      this.lon = coord.getLon();
  }
  
  
  /**
   * Métodos get
   */
  public double getLat() {
      return this.lat;
  }
    
  public double getLon() {
      return this.lon;
  }
  
  /**
   * Métodos set
   */
  public void setLat(double lat) {
      this.lat = lat;
  }
    
  public void setLon(double lon) {
      this.lon = lon;
  }
  
  
  public void deslocamento(double lat, double lon) {
      this.lat += lat;
      this.lon += lon;
  }
    
  public void somaPonto(Localizacao coord) {
      this.lat += coord.getLat();
      this.lon += coord.getLon();
  }
    
  public void movePonto(int lat, int lon) {
      this.lat = lat;
      this.lon = lon;
  }
    
  public boolean ePositivo() {
      return (this.lat > 0 && this.lon > 0);
  }
    
  public double distancia(Localizacao coord) {
      return Math.sqrt(Math.pow(this.lat - coord.getLat(), 2) +
                     Math.pow(this.lon - coord.getLon(), 2));
  }
    
  public boolean iguais(Localizacao coord) {
      return (this.lat == coord.getLat() && this.lon == coord.getLon());
  }   
    
  private boolean latIgualAlon() {
    return (Math.abs(this.lat) == Math.abs(this.lon));
  }
  
  /**
   * toString
   */
  public String toString() {
      return "Coordenadas: --> Latitude = " + this.lat + " -  Longitude = " + this.lon;
  }
  
  /**
   * Equals
   */
  public boolean equals(Object o) {
      if (this == o) return true;
      if ((o == null) || (this.getClass() != o.getClass())) return false;
      Localizacao p = (Localizacao) o;
      return (this.lat == p.getLat() && this.lon == p.getLon());
      
  }
  
  /**
   * Clone
   */
  public Localizacao clone() {
      return new Localizacao(this);    
  }    
}