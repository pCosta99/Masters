package Projeto;

public class Posicao {
    private double x;
    private double y;

    public Posicao(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    @Override
    public String toString() {
        return "Posiçao{" +
                "x=" + x +
                ", y=" + y +
                '}';
    }
}
