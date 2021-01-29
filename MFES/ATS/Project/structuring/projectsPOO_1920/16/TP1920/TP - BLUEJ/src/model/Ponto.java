package src.model;

import java.io.Serializable;

/**
 * Classe que implementa um model.Ponto num plano2D.
 * As coordenadas do model.Ponto são double.
 *
 * @author  MaterialPOO
 * @version 20180212
 */
public class Ponto implements Serializable {

    //variáveis de instância
    private double x;
    private double y;

    /**
     * Construtores da classe model.Ponto.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Ponto.
     */
    public Ponto() {
        this.x = 0;
        this.y = 0;
    }

    /**
     * Construtor parametrizado de model.Ponto.
     * Aceita como parâmetros os valores para cada coordenada.
     */
    public Ponto(double cx, double cy) {
        this.x = cx;
        this.y = cy;
    }



    /**
     * Construtor de cópia de model.Ponto.
     * Aceita como parâmetro outro model.Ponto e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */
    public Ponto(Ponto umPonto) {
        this.x = umPonto.getX();
        this.y = umPonto.getY();
    }

    /**
     * métodos de instância
     */

    /**
     * Devolve o valor da coordenada em x.
     *
     * @return valor da coordenada x.
     */
    public double getX() {
        return this.x;
    }

    /**
     * Devolve o valor da coordenada em y.
     *
     * @return valor da coordenada y.
     */
    public double getY() {
        return this.y;
    }

    /**
     * Actualiza o valor da coordenada em x.
     *
     * @param novoX novo valor da coordenada em X
     */
    public void setX(double novoX) {
        this.x = novoX;
    }

    /**
     * Actualiza o valor da coordenada em y.
     *
     * @param novoY novo valor da coordenada em Y
     */
    public void setY(double novoY) {
        this.y = novoY;
    }

    /**
     * Método que desloca um ponto somando um delta às coordenadas
     * em x e y.
     *
     * @param deltaX valor de deslocamento do x
     * @param deltaY valor de deslocamento do y
     */
    public void deslocamento(double deltaX, double deltaY) {
        this.x += deltaX;
        this.y += deltaY;
    }

    /**
     * Método que soma as componentes do model.Ponto passado como parâmetro.
     * @param umPonto ponto que é somado ao ponto receptor da mensagem.
     */
    public void somaPonto(Ponto umPonto) {
        this.x += umPonto.getX();
        this.y += umPonto.getY();
    }

    /**
     * Método que move o model.Ponto para novas coordenadas.
     * @param cx novo valor de x.
     * @param cy novo valor de y.
     */
    public void movePonto(double cx, double cy) {
        this.x = cx;
        this.y = cy;
    }

    /**
     * Método que determina se o ponto está no quadrante positivo de x e y
     * @return booleano que é verdadeiro se x>0 e y>0
     */
    public boolean ePositivo() {
        return (this.x > 0 && this.y > 0);
    }

    /**
     * Método que determina a distância de um model.Ponto a outro.
     * @param umPonto ponto ao qual se quer determinar a distância
     * @return double com o valor da distância
     */
    public double distancia(Ponto umPonto) {

        return Math.sqrt(Math.pow(this.x - umPonto.getX(), 2) +
                Math.pow(this.y - umPonto.getY(), 2));
    }


    /**
     * Método que determina se dois pontos são iguais.
     * @return booleano que é verdadeiro se os valores das duas
     * coordenadas forem iguais
     */
    public boolean iguais(Ponto umPonto) {
        return (this.x == umPonto.getX() && this.y == umPonto.getY());
    }


    /**
     * Método que determina se o módulo das duas coordenadas é o mesmo.
     * @return true, se as coordenadas em x e y
     * forem iguais em valor absoluto.
     */
    private boolean xIgualAy() {
        return (Math.abs(this.x) == Math.abs(this.y));
    }

    /**
     * Método que devolve a representação em String do model.Ponto.
     * @return String com as coordenadas x e y
     */
    public String toString() {
        return "model.Ponto: --> Cx = " + this.x + " -  Cy = " + this.y;
    }

    public boolean equals(Object o) {
        if (this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        Ponto p = (Ponto) o;
        return (this.x == p.getX() && this.y == p.getY());

    }


    /**
     * Método que faz uma cópia do objecto receptor da mensagem.
     * Para tal invoca o construtor de cópia.
     *
     * @return objecto clone do objecto que recebe a mensagem.
     */

    public Ponto clone() {
        return new Ponto(this);
    }
}