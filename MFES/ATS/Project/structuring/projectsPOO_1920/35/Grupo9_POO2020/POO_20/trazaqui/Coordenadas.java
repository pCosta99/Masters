import java.lang.Math;

public class Coordenadas{
   
   //Variaveis de instância
   private double latitude;
   private double longitude;

   /**
    * Construtor vazio
    */
   public Coordenadas(){
       this.latitude = 0;
       this.longitude = 0;
   }

   /**
    * Construtor parametrizado 
    */
   public Coordenadas(double latitude, double longitude){
       this.latitude = latitude;
       this.longitude = longitude;
   }

   /**
    * Construtor de cópia 
    */
   public Coordenadas(Coordenadas c){
       this.latitude = c.getLatitude();
       this.longitude = c.getLongitude();
   }

   //Getters
    
   public double getLatitude(){
       return this.latitude;
   }

   public double getLongitude(){
       return this.longitude;
   }

   //Setters
    
   public void setLatitude(double latitude){
       this.latitude = latitude;
   }

   public void setLongitude(double longitude){
       this.longitude = longitude;
   }

   /**
    * Metodo Equals
    */
   public boolean equals(Object o) {
       if (this == o)
       return true;
        
       if (o == null || this.getClass() != o.getClass()) 
       return false;
       
       Coordenadas c = (Coordenadas) o;
       
       return this.latitude == c.getLatitude() && 
       this.longitude == c.getLongitude();
    }

   public String toString (){
       StringBuilder sb = new StringBuilder();
       sb.append("Latitude: " + this.getLatitude() + "\n");
       sb.append("Longitude: " + this.getLongitude() + "\n");
       return sb.toString();
   }
 
   //Clone
    
   public Coordenadas clone(){
       return new Coordenadas(this);
   }
    
}