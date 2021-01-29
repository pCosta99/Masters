import java.util.*;
import java.io.Serializable;
public class Utilizador implements Serializable
{
   //Variaveis
   private String codUtilizador;
   private String nome;
   private GPS gps;
   private String pwU;
   
   //Instancias
   public Utilizador(){
    this.codUtilizador = "";
    this.nome = "";
    this.gps = new GPS();
    this.pwU = "";
   }
   
   public Utilizador(String codU,String nome,GPS gps, String pw){
    this.codUtilizador = codU;
    this.nome= nome;
    this.gps = gps;
    this.pwU = pw;
    }
    
   public Utilizador(Utilizador u){
    this.codUtilizador = u.getCodUtilizador();
    this.nome = u.getNome();
    this.gps= u.getGPS();
    this.pwU = u.getPwU();
    }
    
   //Getters 
   public String getCodUtilizador(){
       return this.codUtilizador;
    }
    
   public String getNome(){
       return this.nome;
    }
    
   public GPS getGPS(){
       return this.gps.clone();
    }
    
   public String getPwU(){
       return this.pwU;
    }   
    
   //Setters 
   public void setCodUtilizador(String util){
       this.codUtilizador=util;
    }
    
   public void setNome(String nome){
       this.nome = nome;
    }
    
   public void setGPS(GPS gp){
       this.gps = gp.clone();
    } 
    
   public void setPwU(String pw){
       this.pwU = pw;
    }   
    
   //toString
   public String toString(){
       StringBuilder sb = new StringBuilder();
       sb.append("Codigo: ").append(this.codUtilizador).append("\n").
       append("Nome: ").append(this.nome).append("\n").
       append("Local: ").append(this.gps).append("\n");
       return sb.toString();
       //Nao mostra password
    } 
    
    //boolean
   public boolean equals (Object o){ 
      if (o == this) return true;
      if (o == null || o.getClass() != this.getClass()) return false;
      Utilizador u = (Utilizador) o;
      return this.codUtilizador.equals(u.getCodUtilizador())&&
             this.nome.equals(u.getNome())&&
             this.gps.equals(u.getGPS()) &&
             this.pwU.equals(u.getPwU());
    }
    
   //Clone 
   public Utilizador clone(){
    return new Utilizador(this);
    }
   
   //inserir pedidos de encomendas a uma loja, por parte de um utilizador
   
   public void pedeEncomenda(Encomenda e, Login l){
          l.getPedidosEncomendas().put(this.codUtilizador, e);
   }
    
    
   public boolean aceitaPreco(Encomenda e){
       return true;
   }    
   
   
   //classificar um voluntario
   public void classificaVoluntario(Voluntario v,int classificacao){ 
       v.setClassificacao((v.getClassificacao() + classificacao)/2);
    }
   
   //classificar transportadora
   public void classificaTransportadora( Transportadora t , int classificacao){ 
       t.setClassificacao((t.getClassificacao() + classificacao )/2);
    }
   
   //CSV
   
   public String toStringCSV(){
       StringBuilder sb = new StringBuilder();
       sb.append("Utilizador:")
         .append(this.codUtilizador).append(",")
         .append(this.nome).append(",")
         .append(this.gps.getLongitude()).append(",")
         .append(this.gps.getLatitude()).append(",")
         .append(this.pwU);
       return sb.toString();  
   }   
}