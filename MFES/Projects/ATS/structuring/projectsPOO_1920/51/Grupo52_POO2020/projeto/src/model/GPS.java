package model;

import java.io.Serializable;

/**
 * Classe que implementa um Ponto num plano2D.
 * As coordenadas do Ponto são doubles.
 *
 * @author  Material de Aulas (alterado)
 */
public class GPS implements Serializable {
    private double x;
    private double y;

    /**
     * Construtor por omissão de Ponto.
     */
    public GPS() {
        this.x = 0;
        this.y = 0;
    }

    /**
     * Construtor parametrizado de Ponto.
     * Aceita como parãmetros os valores para cada coordenada.
     */
    public GPS(double cx, double cy) {
        this.x = cx;
        this.y = cy;
    }

    /**
     * Construtor de cópia de Ponto.
     * Aceita como parãmetro outro Ponto e utiliza os métodos de acesso aos valores das variáveis de instância.
     */
    public GPS(GPS umGPS) {
        this.x = umGPS.getX();
        this.y = umGPS.getY();
    }

    /**
     * Método de instância (get).
     * Devolve o valor da coordenada em x.
     *
     * @return valor da coordenada x.
     */
    public double getX() {
        return this.x;
    }

    /**
     * Método de instância (get).
     * Devolve o valor da coordenada em y.
     *
     * @return valor da coordenada y.
     */
    public double getY() {
        return this.y;
    }

    /**
     * Método de instância (set).
     * Atualiza o valor da coordenada em x.
     *
     * @param novoX novo valor da coordenada em X
     */
    public void setX(double novoX) {
        this.x = novoX;
    }

    /**
     * Método de instância (set).
     * Atualiza o valor da coordenada em y.
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
     * Método que soma as componentes do Ponto passado como parãmetro.
     * @param umGPS ponto que é somado ao ponto receptor da mensagem.
     */
    public void somaPonto(GPS umGPS) {
        this.x += umGPS.getX();
        this.y += umGPS.getY();
    }

    /**
     * Metodo que move o Ponto para novas coordenadas.
     * @param cx novo valor de x.
     * @param cy novo valor de y.
     */
    public void movePonto(double cx, double cy) {
        this.x = cx;  // ou setX(cx)
        this.y = cy;  // ou this.setY(cy)
    }

    /**
     * Método que determina se o ponto está no quadrante positivo de x e y
     * @return booleano que dá verdadeiro se x>0 e y>0
     */
    public boolean ePositivo() {
        return (this.x > 0 && this.y > 0);
    }

    /**
     * Método que determina a distÃ¢ncia de um Ponto a outro.
     * @param umGPS ponto ao qual se quer determinar a distância
     * @return double com o valor da distância
     */
    public double distancia(GPS umGPS) {

        return Math.sqrt(Math.pow(this.x - umGPS.getX(), 2) +
                Math.pow(this.y - umGPS.getY(), 2));
    }

    /**
     * Método que determina se dois pontos são iguais.
     * @return booleano que dá verdadeiro se os valores das duas coordenadas forem iguais.
     */
    public boolean iguais(GPS umGPS) {
        return (this.x == umGPS.getX() && this.y == umGPS.getY());
    }

    /**
     * Método que determina se o módulo das duas coordenadas dá o mesmo.
     * @return true, se as coordenadas em x e y forem iguais em valor absoluto.
     */
    private boolean xIgualAy() {
        return (Math.abs(this.x) == Math.abs(this.y));
    }

    /**
     * Método que devolve a representação em String do Ponto.
     * @return String com as coordenadas x e y
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("(").append(this.getX()).append(" , ").append(this.getY()).append(")").append("\n");
        return sb.toString();
    }

    /**
     * Método que compara dois objetos.
     * @return booleano que dá verdadeiro se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        GPS p = (GPS) o;
        return (this.x == p.getX() && this.y == p.getY());

    }

    /**
     * Método que faz uma cópia do ponto receptor da mensagem.
     * @return clone do ponto que recebe a mensagem.
     */
    public GPS clone() {
        return new GPS(this);
    }
}