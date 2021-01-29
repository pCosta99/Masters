
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.io.Serializable;

public class GPS  implements Serializable {
    
    private double x;
    private double y;

    public GPS()
    {
        x = 0;
        y = 0;
    }
    
    public GPS(double x, double y) {
        this.x = x;
        this.y = y;
    }
    
    public GPS(GPS gps) {
        this.x = gps.getX();
        this.y = gps.getY();
    }
    
    public double getX() {
        return this.x;
    }
    
    public double getY(){
        return this.y;
    }
    
    public boolean equals (Object obj) {
        if(this == obj) return true;
        if((obj == null) || (this.getClass() != obj.getClass())) return false;
        GPS gps = (GPS) obj;
        return this.getX() == gps.getX() && this.y == gps.getY();
    }
    
    public String toString() {
        return "X: " + this.getX() + " | " + "Y: " + this.getY();
    }
    
    public GPS clone() {
        return new GPS(this);
    }
}
