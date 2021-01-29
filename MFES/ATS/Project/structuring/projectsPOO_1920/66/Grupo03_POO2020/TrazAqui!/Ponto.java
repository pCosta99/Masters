import java.io.Serializable;

/**
 * Classe que contém informação relativa a uma posição
 */
public class Ponto implements Serializable {
    private double x;
    private double y;

    /**
     * Contrutor vazio
     */
    public Ponto() {
        this.x = 0;
        this.y = 0;
    }

    /**
     * Contrutor com argumentos
     * @param x Abcissa
     * @param y Ordenada
     */
    public Ponto (double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Contrutor com um Ponto
     * @param p Ponto
     */
    public Ponto(Ponto p) {
        this.x = p.x;
        this.y = p.y;
    }

    /**
     * Devolve a abcissa
     * @return Abcissa
     */
    public double getX () {return this.x;}

    /**
     * Devolve a ordenada
     * @return Ordenada
     */
    public double getY () {return this.y;}

    /**
     * Introduz o abcissa
     * @param x abcissa
     */
    public void setX (double x) {this.x = x;}

    /**
     * Introduz o ordenada
     * @param y ordenada
     */
    public void setY (double y) {this.y = y;}

    /**
     * Introduz a abcissa e a ordenada
     * @param x abcissa
     * @param y ordenada
     */
    public void setXY (double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Método clone
     * @return Clone de um Ponto
     */
    public Ponto clone () {
        return new Ponto(this);
    }

    /**
     * Método equals
     * @param o Object
     * @return true se os Pontos forem iguais ou false caso contrário
     */
    public boolean equals (Object o) {
       if (this == o) return true;
       if (o == null || this.getClass()!= o.getClass()) return false;
       Ponto p = (Ponto) o;
       return (this.x == p.getX() && this.y == p.getY());
    }

    /**
     * Método toString
     * @return String com informação relativa a um Ponto
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("X: ").append(this.x).append("\n")
                .append("Y: ").append(this.y).append("\n");
        return sb.toString();
    }

    /**
     * Método que calcula a distância entre dois Pontos
     * @param a Ponto
     * @return Distância
     */
    public double distancia(Ponto a) {
        return Math.sqrt(Math.pow(a.getX()-this.getX(),2) + Math.pow(a.getY()-this.getY(),2));
    }
}
