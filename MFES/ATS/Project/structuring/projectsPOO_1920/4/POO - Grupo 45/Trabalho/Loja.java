import java.util.*;
import java.io.*;


public class Loja implements Serializable
{
    private String nome;
    private String email;
    private String password;
    private Map <String,Encomenda> encomendas;
    private Map <String,Artigo> artigos;
    private float pos_x;
    private float pos_y;

    public Loja(){
        this.nome = new String();
        this.email = new String();
        this.password = new String();
        this.encomendas = new HashMap();
        this.artigos = new HashMap<>();
        this.pos_x = 0;
        this.pos_y = 0;
    }
    
    public Loja(String email, String nome, String password, Map <String, Encomenda> encomendas ,float pos_x, float pos_y, Map<String,Artigo> artigos){
        this.nome = nome;
        this.email = email;
        this.password = password;
        this.encomendas = encomendas;
        this.pos_x = pos_x;
        this.pos_y = pos_y;
        this.artigos = artigos;
    }
    
    public  Loja(Loja l){
        this.nome = l.getNome();
        this.email = l.getEmail();
        this.password = l.getPassword();
        this.encomendas = l.getEncomendas();
        this.pos_x = l.getPos_x();
        this.pos_y = l.getPos_y();
        this.artigos = l.getArtigos();
    }

    public String getNome(){ return this.nome;}

    public String getEmail(){return this.email;}

    public String getPassword(){ return this.password;}
    
    public Map<String,Encomenda> getEncomendas(){return this.encomendas;}

    public float getPos_x() { return this.pos_x;}

    public float getPos_y() { return this.pos_y;}

    public Map<String,Artigo> getArtigos(){ return this.artigos; }

    public void setNome(String nome){ this.nome = nome;}

    public void setEmail(String email){this.email = email;}

    public void setPassword(String password){ this.password = password;}
    
    public void setEncomendas(Map <String, Encomenda> encomendas){this.encomendas = encomendas;}

    public void setPos_x(float pos_x){ this.pos_x = pos_x;}

    public void setPos_y(float pos_y){ this.pos_y = pos_y;}

    public void setArtigos(Map<String,Artigo> artigos){ this.artigos = artigos; }

    public String toString() {
        return "Loja{" +
                "nome='" + nome + '\'' +
                ", email='" + email + '\'' +
                ", encomendas=" + encomendas +
                ", pos_x=" + pos_x +
                ", pos_y=" + pos_y +
                '}';
    }

    public boolean equals (Object obj){
        if(this == obj) return true;
        if ((obj == null) || ( this.getClass()!= obj.getClass())) return false;
        Loja l = (Loja) obj;
        return (this.email.equals(l.getEmail())&&
                this.nome.equals(l.getNome()) &&
                this.password.equals(l.getPassword())&&
                this.encomendas.equals(l.getEncomendas())&&
                this.pos_y == l.getPos_y() &&
                this.pos_x == l.getPos_x());
    }

    public Loja clone(){ return new Loja(this); }

    public void adicionarEncomenda(Encomenda e){
        this.encomendas.put(e.getId(),e);
    }

    public void adicionarArtigo(Artigo a) { this.artigos.put(a.getId(),a); }

}
