package Utilities;

import java.io.Serializable;

/** Classe que representa um ponto no espaço. */
public class Ponto implements Serializable{
    private static final long serialVersionUID = 135L;

    /** Posição na coordenada x. */
    private double x;
    /** Posição na coordenada y. */
    private double y;
    
    /** Construtor por omissão. */
    public Ponto() {
        this.x = 0;
        this.y = 0;
    }

    /**
     * Construtor parametrizado.
     * @param cx coordenada x
     * @param cy coordenada y
     */
    public Ponto(double cx, double cy) {
        this.x = cx;
        this.y = cy;
    }

    /**
     * Construtor por cópia.
     * @param umPonto Ponto que será copiado
     */
    public Ponto(Ponto umPonto) {
        this.x = umPonto.getX();
        this.y = umPonto.getY();
    }


    /**
     * Devolve o valor da coordenada em x.
     * @return valor da coordenada x
     */
    public double getX() {
        return this.x;
    }

    /**
     * Devolve o valor da coordenada em y.
     * @return valor da coordenada y
     */
    public double getY() {
        return this.y;
    }


    /**
     * Atualiza o valor da coordenada em x.
     * @param novoX novo valor da coordenada em X
     */
    public void setX(double novoX) {
        this.x = novoX;
    }

    /**
     * Atualiza o valor da coordenada em y.
     * @param novoY novo valor da coordenada em Y
     */
    public void setY(double novoY) {
        this.y = novoY;
    }


    /**
     * Método que devolve a representação em String do Ponto.
     * @return String com as coordenadas x e y
     */
    public String toString() {
        StringBuilder s = new StringBuilder();

        s.append("(");
        s.append(x);
        s.append(",");
        s.append(y);
        s.append(")");

        return s.toString();
    }

    /** 
     * Método que verifica se o Objeto o é igual ao Ponto para o qual a função é chamada. 
     * @param o Objeto que será comparado
     * @return se o objeto comparado é igual ou não.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;

        Ponto p = (Ponto) o;
        return (this.x == p.getX() && this.y == p.getY());
    }

    /** Método que faz uma cópia do objecto recetor da mensagem. */
    public Ponto clone() {
        return new Ponto(this);    
    } 


    /**
     * Método que desloca um ponto somando um delta às coordenadas em x e y.
     * 
     * @param deltaX valor de deslocamento do x
     * @param deltaY valor de deslocamento do y
     */
    public void deslocamento(double deltaX, double deltaY) {
        this.x += deltaX;
        this.y += deltaY;
    }

    /**
     * Método que soma as componentes do Ponto passado como parâmetro.
     * @param umPonto ponto que é somado ao ponto recetor da mensagem
     */
    public void somaPonto(Ponto umPonto) {
        this.x += umPonto.getX();
        this.y += umPonto.getY();
    }

    /**
     * Método que determina a distância de um Ponto a outro.
     * @param umPonto ponto ao qual se quer determinar a distância
     * @return double com o valor da distância
     */
    public double distancia(Ponto umPonto) {

      return Math.sqrt(Math.pow(this.x - umPonto.getX(), 2) +
                       Math.pow(this.y - umPonto.getY(), 2));
    }

}