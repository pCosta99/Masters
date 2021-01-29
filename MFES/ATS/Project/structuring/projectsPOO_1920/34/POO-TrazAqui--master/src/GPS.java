import java.io.Serializable;
import java.lang.Math; 

public class GPS implements Serializable {
    private double x;
    private double y;

    public GPS() {
        this.x = 0;
        this.y = 0;
    }
    public GPS(GPS g){
        this.x = g.getX();
        this.y = g.getY();
    }
    public GPS(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
    }

    public boolean equals(Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        GPS a = (GPS) o;
        return this.x==(a.getX()) && this.y==(a.getY());
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nEixo X:").append(x).append("\nEixo Y:").append(y).append("\n");
        return sb.toString();
    }

    /**
    *  Método que determina a distância entre duas coordenadas gps. 
    * @return dounle, uma distância.
    */
    public double distancia (GPS gps) {
    	double d = 0;

    	d = Math.sqrt(Math.pow(gps.getX()- this.getX(),2) + Math.pow(gps.getY() - this.getY(), 2));

    	return d;
    }

    public GPS clone(){
        return new GPS(this);
    }
}
