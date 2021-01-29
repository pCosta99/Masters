
import java.util.*;
import java.io.*;

public class Utilizador implements Serializable
{
    private String email;
    private String nome;
    private String password;
    private float pos_x;
    private float pos_y;
    private Map<String,Encomenda> encomendas;
    
    //Construtor vazio
    public Utilizador() {
        this.email= new String();
        this.nome=new String();
        this.password = new String();
        this.pos_x = 0;
        this.pos_y = 0;
        this.encomendas = new HashMap<String,Encomenda>();
    }
    
    //Construtor por parâmetros
    public Utilizador (String email, String nome, String password, float pos_x, float pos_y, Map<String,Encomenda> encomendas){
        this.nome = nome;
        this.email=email;
        this.password=password;
        this.pos_x=pos_x;
        this.pos_y=pos_y;
        this.encomendas = encomendas;
    }
    
    public Utilizador ( Utilizador u){
        this.nome = u.getNome();
        this.email=u.getEmail();
        this.password=u.getPassword();
        this.pos_x=u.getPos_x();
        this.pos_y=u.getPos_y();
        this.encomendas=u.getEncomendas();
    }
    
    //métodos 
    //Gets
    public String getNome(){
        return this.nome;
    }
 
    public String getEmail(){
        return this.email;
    }
    public String getPassword(){
        return this.password;
    }
    
    public float getPos_x(){
        return this.pos_x;
    }
    
    public float getPos_y(){
        return this.pos_y;
    }

    public Map<String,Encomenda> getEncomendas(){ return this.encomendas; }
    
    //sets
    
    public void setNome(String nome){
        this.nome=nome;
    }
    public void setEmail(String email){
        this.email=email;
    }
    public void setPassword(String password){
        this.password=password;
    }
    
    public void setPos_x(float pos_x){
        this.pos_x=pos_x;
    }
    
    public void setPos_y(float pos_y){
        this.pos_y=pos_y;
    } 
    
    public void setEncomendas(Map encomendas){
        this.encomendas = encomendas;
    }

    public String toString() {
        StringBuilder s = new StringBuilder ();
        s.append("\tNome: " + this.nome + "\n");
        s.append("\tEmail: " + this.email + "\n");
        s.append("\tPassword: " + this.password + "\n");
        s.append("Posição: " + this.pos_x + " " + this.pos_y);
        return s.toString();
    }
    
    public boolean equals (Object obj){
        if(this == obj) return true;
        if ((obj == null) || ( this.getClass()!= obj.getClass())) return false;
        Utilizador umUser =(Utilizador) obj;
        return (this.email.equals(umUser.getEmail())&&
                 this.nome.equals(umUser.getNome()) &&
                 this.password.equals(umUser.getPassword())&&
                 this.pos_y == umUser.getPos_y() &&
                 this.pos_x == umUser.getPos_x());
    }
    
    public Utilizador clone(){
        return new Utilizador(this);
    }
    
    public void adicionarEncomenda(Encomenda e){
        this.encomendas.put(e.getId(),e);
    }

}
