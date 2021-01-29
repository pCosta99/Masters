import java.io.Serializable;
import java.util.List;

import static java.lang.Math.pow;
import static java.lang.Math.sqrt;

public class Coordenadas implements Serializable {
    private double x;
    private double y;

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

    public Coordenadas(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public Coordenadas(Coordenadas a){
        this.x=a.getX();
        this.y=a.getY();
    }

    public Coordenadas clone (){
        return new Coordenadas(this);
    }

    /** Método que calcula a distância que a coordenada representa
     *
     * @param a Coordenadas
     */
    public double distancia (Coordenadas a){
        return sqrt(pow((a.getX()-this.x),2) + pow((a.getY()-this.y),2));
    }

    /** Método que retorna se está no range
     *
     * @param a Coordenadas
     * @param b Distância
     */
    public boolean isRange (Coordenadas a,double b){
        return (a.distancia(this)<=b);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("X: ").append(this.x).append("- Y:")
                .append(this.y);
        return sb.toString();
    }
}