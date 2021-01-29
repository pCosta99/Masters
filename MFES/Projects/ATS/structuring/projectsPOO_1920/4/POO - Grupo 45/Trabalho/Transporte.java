import java.util.*;
import java.io.*;

public abstract class Transporte implements Serializable
{
    private String nome;
    private String email;
    private String password;
    private double raio;
    private float pos_x;
    private float pos_y;
    private List <Integer> rating;
    private List <Viagem> viagens;
    private boolean disp;
    
    public Transporte(){
        this.nome = new String();
        this.email = new String();
        this.password = new String();
        this.raio = 0;
        this.pos_x=0;
        this.pos_y=0;
        this.rating = new ArrayList<Integer> ();
        this.viagens = new ArrayList<Viagem> ();
        this.disp = false;

    }
    
    public Transporte(String email, String nome, String password, float pos_x, float pos_y, double raio,boolean disp) {
        this.nome=nome;
        this.email=email;
        this.password=password;
        this.pos_x=pos_x;
        this.pos_y=pos_y;
        this.raio=raio;
        this.rating=new ArrayList<Integer>();
        this.viagens = new ArrayList<Viagem> ();
        this.disp = disp;
    }
    
    public Transporte(Transporte t){
        this.nome=t.getNome();
        this.email=t.getEmail();
        this.password=t.getPassword();
        this.pos_x=t.getPos_x();
        this.pos_y=t.getPos_y();
        this.raio=t.getRaio();
        this.rating=t.getRating();
        this.disp = t.getDisp();
        this.viagens = t.getViagens();
    }
    
    public String getNome(){ return this.nome;}
    
    public String getEmail(){return this.email;}

    public String getPassword(){ return this.password;}
    
    public boolean getDisp(){return this.disp;}
    
    public double getRaio() { return this.raio;}
        
    public float getPos_x() { return this.pos_x;}

    public float getPos_y() { return this.pos_y;}

    public List<Integer> getRating() { return this.rating; }

    public List<Viagem> getViagens() {return this.viagens;}


    public void setNome(String nome){ this.nome = nome;}
    
    public void setEmail(String email){this.email = email;}
 
    public void setPassword(String password){ this.password = password;}
    
    public void setRaio(double raio){ this.raio = raio;}
    
    public void setPos_x(float pos_x){ this.pos_x = pos_x;}
    
    public void setPos_y(float pos_y){ this.pos_y = pos_y;}

    public void setRating(List <Integer> rating) { this.rating = rating; }
    
    public void setDisp (boolean disp){this.disp = disp;}

    public void setViagens(List<Viagem> viagens) { this.viagens = viagens; }

    public boolean equals (Object obj){
        if(this == obj) return true;
        if ((obj == null) || ( this.getClass()!= obj.getClass())) return false;
        Transporte t = (Transporte) obj;
        return (this.email.equals(t.getEmail())&&
                this.nome.equals(t.getNome()) &&
                this.password.equals(t.getPassword())&&
                this.pos_y == t.getPos_y() &&
                this.pos_x == t.getPos_x()) &&
                this.raio == t.getRaio() &&
                this.rating.equals(t.getRating()) &&
                this.disp == t.getDisp()&&
                this.viagens.equals(t.getViagens());
    }
            
    public boolean registarViagem(Viagem novaViagem){
 
        if(!this.viagens.contains(novaViagem))
            this.viagens.add(novaViagem);
            
        else return false;
        
        return true;
    }
    
    public boolean registarClassificacao(int classificacao){
 
        if(!this.rating.contains(classificacao))
            this.rating.add(classificacao);
            
        else return false;
        
        return true;
    }
    public double calRating(){
        int sum = 0;
        if(this.rating.isEmpty()){
            return 0;
        }
        else{
            for(Integer i : this.rating){
                sum+=i;
            }
            return sum/this.rating.size();
        }
    }

    public String toString() {
        return "Transporte{" +
                "nome='" + nome + '\'' +
                ", email='" + email + '\'' +
                ", password='" + password + '\'' +
                ", raio=" + raio +
                ", pos_x=" + pos_x +
                ", pos_y=" + pos_y +
                ", rating=" + this.calRating() +
                ", disponbilidade=" + disp+ 
                ", viagens=" + viagens +
                '}';
    }
}
