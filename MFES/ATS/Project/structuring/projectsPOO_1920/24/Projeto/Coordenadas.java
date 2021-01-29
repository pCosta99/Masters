import java.io.Serializable;




public class Coordenadas implements Serializable
{
    private double lat;
    private double lon;
    
    
  public Coordenadas() {
      this.lat = 0;
      this.lon = 0;
  }
  
  public Coordenadas(double nLAT, double nLON){
      this.lat = nLAT;
      this.lon = nLON;
  }
  
  public Coordenadas(Coordenadas c){
      this.lat = c.getLat();
      this.lon = c.getLon();
  
    }
    
  public boolean equals(Object o) {
    if (this == o)
        return true;
    if ((o == null) || (this.getClass() != o.getClass()))
        return false;
    Coordenadas p = (Coordenadas) o;
    return (this.lat == p.getLat() && this.lon == p.getLon());
      
  }
  
  public String toString() { /* CSV  */
      return this.lat + "," + this.lon;
  }
  
  public Coordenadas clone() {
      return new Coordenadas(this);    
  }
   
   
  
    // 1 a)
  public double getLat(){
      return (this.lat);
        
    }
    // 1 b)
  public double getLon(){
      return (this.lon);
    }
    // 1 d)
  public void setLat(double novoLat){
      (this.lat)=novoLat;
    }
    
  public void setLon(double novoLon){
      (this.lon)=novoLon;
    }
    
    // 1 e)
  public void alteraCentro(double nLat, double nLon){
      (this.lat)=nLat;
      (this.lon)=nLon;
    }

    public static double distance (Coordenadas cord1, Coordenadas cord2){
      double lat1=cord1.getLat();
      double lon1=cord1.getLon();
      double lat2=cord2.getLat();
      double lon2=cord2.getLon();
      return Math.sqrt((lat2 - lat1) * (lat2 - lat1) + (lon2 - lon1) * (lon2 - lon1));
  }

}

