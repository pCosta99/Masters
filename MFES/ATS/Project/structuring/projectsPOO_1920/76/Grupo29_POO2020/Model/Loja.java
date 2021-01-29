package Model;

import Utilities.Ponto;

/**
*   @class Loja define uma loja registada na classe @class TrazAqui. 
*/
public class Loja extends Entidade implements ILoja{
    private static final long serialVersionUID = 130L;
    private int emFila;

    /*
    *   Contrutor vazio cria uma loja sem código, nome, com um construtor de Ponto vazio e 0 em fila.
    */
    public Loja(){
        super();
        this.emFila = 0;
    }

    /*
    *  Simplesmente insere os parâmetros nas instância respetivas.
    */
    public Loja(String id, String nome, Ponto local, int emFila){
        super(id, nome, local);
        this.emFila = emFila;
    }

    /*
    *  Simplesmente insere os parâmetros da @param l @class Loja nas instância respetivas.
    */
    public Loja(Loja l){
        super(l);
        this.emFila = l.getEmFila();
    }

    /*
    *  Define @param emFila como o número de utilizadores em fila de uma @class Loja.
    */
    public void setEmFila(int emFila){
        this.emFila = emFila;
    }

    /*
    * @return o número de utilizadores em fila de uma @class Loja.
    */
    public int getEmFila(){
        return this.emFila;
    }

    /*
    * Torna uma @class Loja numa @class String. 
    */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Loja [");
        sb.append(super.getId());
        sb.append("]\n");
        sb.append("\tNome: ");
        sb.append(super.getNome());
        sb.append("\n\tLocal: ");
        sb.append(super.getPosicao());
        sb.append("\n\tEm fila: ");
        sb.append(this.emFila);

        return sb.toString();
    }

    /*
    *  Clona uma @class Loja.
    */
    public Loja clone(){
        return new Loja(this);
    }

    /*
    *  Determina se a @class Loja é igual ao @param obj da classe @class Object genérica.
    */
    public boolean equals(Object obj){
        if(! super.equals(obj)) return true;
        if(this.getClass() != obj.getClass()) return false;
        Loja l = (Loja) obj;
        return l.getEmFila() == this.emFila;  
    }

    public void incrementa(){ this.emFila += 1;}

    public void decrementa(){ this.emFila -= 1;}

}