import java.io.*;

public class Administrador implements Serializable{
    /**Nome do Administrador que permite ter acesso as definiçoes do sistema*/
    private String nome;
    /**Password do administrador*/
    private String password;
    
    /**Construtor vazio do Administrador*/
    public Administrador(){
        nome = "";
        password = "";
    }
    
    /**Construtor com as variaveis do Administrador*/
    public Administrador(String nome, String pass){
        this.nome = nome;
        this.password = pass;
    }
    
    /**Construtor do Administrador com um Administrador*/
    public Administrador(Administrador admin){
        nome = admin.getNome();
        password = admin.getPassword();
    }
    
     /**
     * Metodo que retorna o nome do administrador
     * 
     * @return nome do administrador
     */
    public String getNome(){
        return nome;
    }
     /**
     * Metodo que retorna a password do administrador
     * 
     * @return password do administrador
     */
    public String getPassword(){
        return password;
    }
    
     /**
     * Metodo que altera o nome do administrador
     * 
     * @param name Novo nome do administrador
     */
    public void setNome(String name){
        this.nome = name;
    }
    
     /**
     * Metodo que altera a password do administrador
     * 
     * @param pass Nova password do administrador
     */
    public void setPassword(String pass){
        password = pass;
    }
    
     /**
     * Metodo que retorna uma copia do aministrador
     * 
     * @return Copia do Administrador
     */
    public Administrador clone(){
        return new Administrador(this);
    }
    
     /**
     * Metodo que transforma o administrador numa String
     * 
     * @return String do administrador
     */
    public String toString(){
        return nome + " " + password;
    }
    
     /**
     * Metodo que compara um objeto com um administrador
     * 
     * @param o Objeto que ira ser comparado
     * 
     * @return true caso o objeto e o administrador sejam iguais, false caso contrario
     */
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        Administrador admin = (Administrador) o;
        if(nome.equals(admin.getNome()) && password.equals(admin.getPassword())) return true;
        return false;
    }
}