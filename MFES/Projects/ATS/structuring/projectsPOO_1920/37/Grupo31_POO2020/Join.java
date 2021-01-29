
/**
 * Escreva a descrição da classe Join aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */ 
import java.io.*;
public abstract class Join implements Serializable{
    /** Coordenadas no momento */
    private Localizacao localizacao;
    private String user;
    private String pass;
    
    public Join(){
      this.localizacao = new Localizacao();
      this.user="";
      this.pass="";
    }
    
    public Join(Localizacao localizacao, String user, String pass){
        this.localizacao = localizacao;
        this.user=user;
        this.pass=pass;
    }
    
    public Join(Join j){
        this.localizacao =j.getLocalizacao();
        this.user = j.getUser();
        this.pass = j.getPass();
    }
    
    /**
     * Get
     */
    
    public Localizacao getLocalizacao(){
        return this.localizacao;
    }
    
    
    public String getUser(){
        return this.user;
    }
    
    public String getPass(){
        return this.pass;
    }
    
    /**
     * Set
     */
    public void setLocalizacao(Localizacao localizacao){
        this.localizacao = localizacao;
    }
    
    
    public void setUser(String user){
        this.user = user;
    }
    
    public void setPass(String pass){
        this.pass = pass;
    }
    
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Utilizador that = (Utilizador) obj;
        return that.getLocalizacao().equals(this.localizacao) &&
                that.getUser().equals(this.user) &&
               that.getPass().equals(this.pass);
    }
    
    
    public abstract String toString(); 
       
    public abstract Join clone();
    
}
