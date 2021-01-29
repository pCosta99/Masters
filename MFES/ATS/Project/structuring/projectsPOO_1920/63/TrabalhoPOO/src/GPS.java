import java.io.Serializable;
import java.util.List;

public class GPS implements Serializable {
    /** variaveis de instancia */
    private double x;
    private double y;

    /** constructores de classe */
    /** vazio */
    public GPS(){
        this.x = 0.0;
        this.y = 0.0;
    }

    /** parametrico */
    public GPS(double newGpsX, double newGpsY){
        this.x = newGpsX;
        this.y = newGpsY;
    }

    /** copia */
    public GPS(GPS newGPS){
        this.x = newGPS.getX();
        this.y = newGPS.getY();
    }

    /** gets/sets das variaveis de instancia */
    public double getX(){ return this.x; }
    public void setX(double newGpsX) { this.x = newGpsX; }

    public double getY(){ return this.y; }
    public void setY(double newGpsY) { this.y = newGpsY; }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        GPS passed = (GPS) o;
        return (this.x == passed.x &&
                this.y == passed.y);
    }

    public String toString(){
        return Double.toString(this.x) + "," + Double.toString(this.y);
    }

    public GPS clone(){
        return new GPS(this);
    }

    /** metodos especificos */

    public double distancia(GPS gps){
        double res;
        res = Math.sqrt(Math.pow((gps.getX() - this.x),2) + Math.pow((gps.getY() - this.y),2));
        return res;
    }

}
