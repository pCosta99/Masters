import java.io.*;

public class Utilizador extends Entidade implements Serializable
{
     /** Construtor vazio de Utilizador*/
    public Utilizador(){
        super();
    }
    
     /** Construtor com todas as variaveis de instancia do Utilizador*/
    public Utilizador(String cu, String nu, double gpsx, double gpsy, String e, String pass){
        super(cu,nu,gpsx,gpsy,e,pass);
    }
    
     /** Construtor com um Utilizador*/
    public Utilizador(Utilizador u){
        super(u);
    }
    
    /** 
     * Metodo retorna uma copia do Utilizador
     * 
     * @return Uma copia do Utilizador
     */
    public Utilizador clone(){
        return new Utilizador(this);
    }
    
     /** 
     * Metodo que "transforma" um Utilizador numa String
     * 
     * @return Uma String do Utilizador
     */
    public String toString(){
        String s = "Código: " + getCod() + 
        "\nNome: " + getNome() + 
        "\nCoordenadas: " + getX() + ", " + getY() +
        "\nEmail: " + getEmail() +
        "\nPassword: " + getPassword();
        
        return s;
    }
    
     /** 
     * Metodo que verifica se um Objeto é igual a um Utilizador
     * 
     * @param o Objeto que irá ser comparado com o utilizador
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
        
        Utilizador u = (Utilizador) o;
        
        if(super.equals(u)) 
        return true;
        
        return b;
    }
}
