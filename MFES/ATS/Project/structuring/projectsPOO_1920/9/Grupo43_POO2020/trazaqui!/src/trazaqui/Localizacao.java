package trazaqui;

import java.io.Serializable;

public class Localizacao implements Serializable {
    //variáveis de instância
    private double x;
    private double y;

    /**
     * Construtor vazio para classe Localizacao; Inicializa (0,0)
     */
    public Localizacao()
    {
        this.x= 0;
        this.y= 0;
    }

    /**
     * Construtor parametrizado da classe Localizacao
     * @param x A coordenada x
     * @param y A coordenada y
     */
    public Localizacao(double x, double y){
        this.x = x;
        this.y = y;
    }

    /**
     * Construtor de cópia de um objecto da classe Localizacao
     * @param b A Localizacao a copiar
     */
    public Localizacao(Localizacao b){
        this.x = b.getX();
        this.y = b.getY();
    }

    //definimos os métodos

    /**
     * Método para devolver a coordenada X
     * @return A coordenada x
     */
    public double getX(){
        return this.x;
    }

    /**
     * Método para devolver a coordenada Y
     * @return A coordenada y
     */
    public double getY(){
        return this.y;
    }

    /**
     * Método para actualizar a coordenada X
     * @param n A coordenada x
     */
    public void setX(double n){
        this.x = n;
    }
    /**
     * Método para actualizar a coordenada Y
     * @param n A coordenada y
     */
    public void setY(double n){
        this.y = n;
    }
    /**
     * Método para clonar um objecto Localizacao
     */
    public Localizacao clone(){
        return new Localizacao(this);
    }

    /**
     * Método para devolver a informação sobre o objecto
     * @return Uma string com a informação
     */
    public String toString(){
        return "X = " + x + " ; Y = " + y;
    }

    /**
     * Método  para devolver a localização
     * @return Uma localização
     */
    public Localizacao getLocalizacao(){
        return new Localizacao(this.getX(),this.getY());
    }

    /**
     * Método para verificar se dois objectos Localização são iguais
     */
    public boolean equals(Object o){
        if(this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;

        Localizacao p = (Localizacao) o;
        return p.getX() == this.x && p.getY() == this.y;
    }

    /**
     * Método para calcular a distância de uma localização a outra
     * @param b A localização para a qual se quer calcular a distância
     * @return A distância
     */
    public double distLocalizacao(Localizacao b) {
        return Math.abs(Math.sqrt(Math.pow((b.getX() - this.getX()),2) + Math.pow((b.getY() - this.getY()),2)));
    }

}

