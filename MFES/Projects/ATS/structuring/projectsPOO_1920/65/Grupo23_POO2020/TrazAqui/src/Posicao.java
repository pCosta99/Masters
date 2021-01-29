import java.io.Serializable;
import java.util.Objects;

/**
 * Classe auxiliar de posiçoes
 */
public class Posicao implements Serializable {
    private double x; // longitude
    private double y; // latitude

    /**
     * Construtores
     */

    /**
     * Por omissão
     */
    public Posicao() {
        this.x = 0;
        this.y = 0;
    }

    /**
     * Parametrizado
     */
    public Posicao(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * De cópia
     */
    public Posicao(Posicao pos) {
        this.x = pos.getX();
        this.y = pos.getY();
    }

    /**
     * Getters
     */
    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    /**
     * Setters
     */
    public void setX(double x) {
        this.x = x;
    }

    public void setY(double y) {
        this.y = y;
    }

    /**
     * Manipulação
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Posicao posicao = (Posicao) o;
        return x == posicao.getX() &&
                y == posicao.getY();
    }

    public int hashCode() { // exemplo: se a.equals(b), então a.hashCode() == b.hashCode()
        return Objects.hash(x, y);
    }

    public String toString() {
        return "Posição(" + x + "," + y + ')';
    }

    public Posicao clone(){
        return new Posicao(this);
    }

    /**
     * Calcula a distância entre this e outra Posicao.
     */
    public double distancia(Posicao p) {
        return Math.sqrt(Math.pow(p.getX() - this.x, 2) +
                Math.pow(p.getY() - this.y, 2));
    }

    /**
     * Determina se duas posições são iguais.
     */
    public boolean iguais(Posicao p) {
        return (this.x == p.getX() && this.y == p.getY());
    }

    /**
     * Método que desloca uma posição somando um delta às coordenadas x e y.
     */
    public void deslocamento(double deltaX, double deltaY) {
        this.x += deltaX;
        this.y += deltaY;
    }

}
