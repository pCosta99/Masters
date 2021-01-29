import java.util.*;
import java.io.*;


public class Utilizador implements Serializable
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private String nome;
    private String email;
    private String password;
    private Localizacao localizacao;
    

    /**
     * COnstrutor para objetos da classe Utilizador
     */
    public Utilizador()
    {
     Localizacao p=new Localizacao();
     this.nome="";
     this.email="";
     this.password="";
     this.localizacao=p;
    }

    
      public Utilizador(Utilizador a) {
        this.email = a.getEmail();
        this.nome = a.getNome();
        this.password = a.getPassword();
        this.localizacao=a.getLocalizacao();
       
    }  
    
    
    public Utilizador(String email,String nome,String password,Localizacao localizacao){
        this.email=email;
        this.nome=nome;
        this.password=password;
        this.localizacao=localizacao;

}
 


// getters

 public String getEmail() { 
        return this.email; 
    }
    
    public String getNome() {
        return this.nome;
    }
    
    public String getPassword() {
        return this.password;
    }
    
  public Localizacao getLocalizacao(){
      return this.localizacao;
    }

   
    
    // setters 
    
      public void setEmail(String email) {
        this.email = email;
    }
    
    public void setNome(String nome) {
        this.nome = nome;
    }
    
    public void setPassword(String password) {
        this.password = password;
    }
    
    public void setLocalizacao(Localizacao l) {
        this.localizacao = l;
    }
    
    
    
    
    public boolean equals (Object o){
        if(this==o) return true;
        
        if((o==null) || (this.getClass()!=o.getClass())) return false;
        
        Utilizador u = (Utilizador) o;
        return(this.nome.equals(u.getNome()) && this.email.equals(u.getEmail())
            &&this.password.equals(u.getPassword()) && this.localizacao.equals(u.getLocalizacao()));
            
    }
    
    public String toString() {
        String s = new String();
 
        s = ("Utilizador: \n" + 
                "Email: " + this.email + "\n"+
               "Nome: " + this.nome + "\n" + 
               "Password: " + this.password + "\n" +
               "Localizacao: "+ this.getLocalizacao() + "\n");
             
 
        return s;
    }
    
    public Utilizador clone() {
        return new Utilizador(this);
    }
}

