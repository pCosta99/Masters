import java.lang.Math;
import java.io.Serializable;
/**
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */
public class Coordenadas implements Serializable{
    private double longitude;
    private double latitude;

    /**
     * Construtores
     */
    public Coordenadas(){
        this.longitude = 0.0;
        this.latitude = 0.0;
    }
    
    public Coordenadas(double latitude, double longitude){
        this.longitude = longitude;
        this.latitude = latitude;
    }
    
    public Coordenadas(Coordenadas g){
        this.longitude = g.getLongitude();
        this.latitude = g.getLatitude();
    } 
    
    /**
     * Get's
     */
    public double getLongitude(){
        return this.longitude;
    }
    public double getLatitude(){
        return this.latitude;
    }
    /**
     * Sets
     */
    public void setLongitude(double longitude){
        this.longitude = longitude;
    }
    public void setLatitude(double latitude){
        this.latitude = latitude;
    }
    
    /**
     * clone
     */
    public Coordenadas clone(){
        return new Coordenadas(this);
    }
    
    /**
     * equals
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Coordenadas l = (Coordenadas) obj;
        return l.getLongitude() == this.longitude &&
               l.getLatitude() == this.latitude;
    }
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Longitude: ").append(this.longitude);
        sb.append("Latitude: ").append(this.latitude);
        return sb.toString();
    }
    
    /**
     * Método que calcula a distância entre dois pontos
     */
    public double distancia(Coordenadas ponto){
        return Math.sqrt(Math.pow(this.longitude - ponto.getLongitude(), 2) + Math.pow(this.latitude - ponto.getLatitude(), 2));
    }
}
