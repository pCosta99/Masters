package Models;

import java.io.Serializable;

public class GPS implements Serializable {
    private double x;
    private double y;

    /**
     * COnstrutor por cópia.
     */
    public GPS(){
        this.x = 0;
        this.y = 0;
    }

    /**
     * Construtor parametrizado.
     * @param x Recebe um double que representa a latitude.
     * @param y Recebe um double que representa a altitude.
     */
    public GPS(double x, double y){
        this.x = x;
        this.y = y;
    }

    /**
     * Construtor por cópia.
     * @param g
     */
    public GPS(GPS g){
        this.x = g.getX();
        this.y = g.getY();
    }

    /**
     * Método que dá o valor da latitude.
     * @return Devolve esse valor.
     */
    public double getX(){
        return this.x;
    }

    /**
     * Método que define o valor da latitude.
     * @return Devolve esse valor.
     */
    public void setX(double x){
        this.x = x;
    }

    /**
     * Método que dá o valor da longitude.
     * @return Devolve esse valor.
     */
    public double getY(){
        return this.y;
    }

    /**
     * Método que define o valor da latitude.
     * @return Devolve esse valor.
     */
    public void setY(double y){
        this.y = y;
    }

    /**
     * Função que verifica se o objeto recebido é idêntico ao da classe GPS.
     * @param o Rece um objeto.
     * @return Devolve um boolean que correponde à verificação.
     */
    @Override
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        GPS le = (GPS) o;
        return le.getX()==(this.x) &&
                le.getY()==(this.y);
    }

    /**
     * Função que traduz a classe GPS.
     * @return Devolve uma String com a respetiva tradução.
     */
    @Override
    public String toString(){
       StringBuilder sb = new StringBuilder();
       sb.append("Latitude: ").append(this.x)
               .append("\nLongitude: ").append(this.y);
       return sb.toString();
    }

    /**
     * Função que faz um clone da classe GPS.
     * @return Devolve esse clone.
     */
    @Override
    public GPS clone(){
        return new GPS(this);
    }
}
