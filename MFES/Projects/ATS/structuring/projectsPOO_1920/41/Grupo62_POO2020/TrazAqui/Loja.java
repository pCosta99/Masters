import java.io.*;

public class Loja extends Entidade implements Serializable
{   
     /** Quantidade de pessoas em fila de espera */
     private int fila;
     
     /** Construtor vazio de Loja*/
    public Loja(){
        super();
        this.fila = 0;
    }
    
     /** Construtor com todas as variaveis de instancia da Loja*/
    public Loja(String cu, String nu, double gpsx, double gpsy, String e, String pass, int fila){
        super(cu,nu,gpsx,gpsy,e,pass);
        this.fila = fila;
    }
    
     /** Construtor com um Loja*/
    public Loja(Loja l){
        super(l);
        this.fila = l.fila;
    }

    public int getFila(){
        return this.fila;
    }

    public void setFila(int fila){
        this.fila = fila;
    }
    
    /** 
     * Metodo retorna uma copia da Loja
     * 
     * @return Uma copia da Loja
     */
    public Loja clone(){
        return new Loja(this);
    }
    
    /** 
     * Metodo que "transforma" uma Loja numa String
     * 
     * @return Uma String da Loja
     */
    public String toString(){
        String s = "Código: " + getCod() + 
        "\nNome: " + getNome() +
        "\nCoordenadas: " + getX() + ", " + getY() +
        "\nEmail: " + getEmail() +
        "\nPassword: " + getPassword() +
        "\nFila: " + fila + " pessoas";
        
        return s;
    }
    
     /** 
     * Metodo que verifica se um Objeto é igual a uma Loja
     * 
     * @param o Objeto que irá ser comparado com a loja
     * 
     * @return true se forem iguais, false caso contrário
     */
    public boolean equals(Object o){
        
        boolean b = false;
        if(this == o){
            return true;
        }
        
        if(o == null || this.getClass() != o.getClass()){
            return false;
        }
        
        Loja l = (Loja) o;
        
        if(super.equals(l) && this.fila == l.getFila()) 
        return true;
        
        return b;
    }
    
    public double tempoEspera(){
        int i = getFila();
        double r = Double.valueOf(i);
        return r;
    }
}
