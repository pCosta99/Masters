import java.util.*;
import java.io.Serializable;
public class Loja implements Serializable
{
  private String codLoja;
  private String nomeL;
  private GPS gpsL;
  private String pwL;
  
  
  public Loja(){ 
    this.codLoja = "n/a";
    this.nomeL = "n/a";
    this.gpsL = new GPS();
    this.pwL = "0000";
    }
  public Loja(String codigo,String nome,GPS gps, String pw){
    this.codLoja = codigo;
    this.nomeL = nome;
    this.gpsL = gps.clone();
    this.pwL = pw;
    }
  public Loja(Loja l){ 
   this.codLoja = l.getCodLoja();
   this.nomeL = l.getNomeL();
   this.gpsL = l.getGPSL();
   this.pwL = l.getPwL();
    }
  public String getCodLoja(){ 
   return this.codLoja;
    }
  public String getNomeL(){ 
   return this.nomeL;
    }
  public GPS getGPSL(){ 
    return this.gpsL.clone();
    }
  public String getPwL(){
    return this.pwL;
    }   
  
  public void setCodLoja(String codigo){ 
    this.codLoja = codigo;
    }
  public void setNomeL(String nome){ 
    this.nomeL = nome;
    }
  public void setGPSL(GPS gps){ 
    this.gpsL = gps.clone();
    }
  public void setPwL(String pw){
    this.pwL = pw;  
    }    
  
  public String toString(){
       StringBuilder sb = new StringBuilder();
       sb.append("Codigo: ").append(this.codLoja).append("\n").
       append("Nome: ").append(this.nomeL).append("\n").
       append("Local: ").append(this.gpsL).append("\n");
       return sb.toString();
       //Nao mostra password
    } 
  public boolean equals (Object o){ 
       if (o == this) return true;
      if (o == null || o.getClass() != this.getClass()) return false;
      Loja l = (Loja) o;
      return this.codLoja.equals(l.getCodLoja())&&
             this.nomeL.equals(l.getNomeL())&&
             this.gpsL.equals(l.getGPSL()) &&
             this.pwL.equals(l.getPwL());
    } 
  public Loja clone(){ 
    return new Loja (this);
    }
    
 

   
  //toStringCSV
  
  public String toStringCSV(){
       StringBuilder sb = new StringBuilder();
       sb.append("Loja:")
         .append(this.codLoja).append(",")
         .append(this.nomeL).append(",")
         .append(this.gpsL.getLongitude()).append(",")
         .append(this.gpsL.getLatitude()).append(",")
         .append(this.pwL);
       return sb.toString();  
   }    
}