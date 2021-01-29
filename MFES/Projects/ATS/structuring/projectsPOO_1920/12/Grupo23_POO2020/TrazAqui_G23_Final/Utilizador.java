import java.util.*; 
import java.nio.charset.StandardCharsets; 
import java.nio.file.*; 
import java.io.*;

/**
 * Superclasse Utilizador :: Todo o Comprador, Voluntário, Loja e Empresa 
 * não passam de utilizadores "especiais" da aplicação. 
 * Assim, partilham algumas informações em comum antes de divergirem.
 */
public abstract class Utilizador implements Serializable
{
    private String email;
    private String pass;
    private String name;
    private Localizacao coord;
    
    public Utilizador(){
        this.email = new String();
        this.pass = new String();
        this.name = new String();
        this.coord = new Localizacao();
    }
    
    public Utilizador(Utilizador u){
        this.email = u.getEmail();
        this.pass = u.getPass();
        this.name = u.getName();
        this.coord = u.getCoord();
    }
    
    public Utilizador(String email, String pass, String name, Localizacao coord){
        this.setEmail(email);
        this.setPass(pass);
        this.setName(name);
        this.setCoord(coord);
    }
    
    /**
     * Métodos get
     */
    public String getEmail(){
        return this.email;
    }
    
    public String getPass(){
        return this.pass;
    }
    
    public String getName(){
        return this.name;
    }
    
    public Localizacao getCoord(){
        return this.coord;
    }
    
    /**
     * Métodos set
     */
    public void setEmail(String mail){
        this.email = mail;
    }
    
    public void setPass(String pass){
        this.pass = pass;
    }
    
    public void setName(String name){
        this.name = name;
    }
    
    public void setCoord(Localizacao coord){
        this.coord = coord;
    }
    
    /**
     * Clone por abstração
     */
    public abstract Utilizador clone();

    /**
     * Equals
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Utilizador u = (Utilizador) o;
        return (this.email.equals(u.getEmail()) &&
                this.pass.equals(u.getPass()) &&
                this.name.equals(u.getName()) &&
                this.coord.equals(u.getCoord()));
    }
    
    /**
     * toString
     */
    public String toString(){
        return ("User email: " + this.getEmail() + " /Name: "+this.getName()
                            + " /Coordernadas: " + this.getCoord());
    }
    
    /**
     * toFile por abstração
     */
    public abstract String stringtoFile();
}