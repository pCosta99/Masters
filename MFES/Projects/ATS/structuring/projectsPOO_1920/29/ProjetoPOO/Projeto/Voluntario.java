import java.util.*;
import java.io.Serializable;
public class Voluntario implements Serializable
{
  //Variavies  
  private String codVoluntario;
  private String nomeV;
  private GPS gpsV;
  private double raio;
  private String pwV;
  private int classificacao;
 
  
  //Instancias
  public Voluntario(){ 
    this.codVoluntario= "n/a";
    this.nomeV = "n/a";
    this.gpsV = new GPS();
    this.raio= 0.0;
    this.pwV = "0000";
    this.classificacao = 0;
  }
  
  public Voluntario(String codigo,String nome,GPS gps,double raio, String pw,int classi){
    this.codVoluntario=codigo;
    this.nomeV = nome;
    this.gpsV = gps.clone();
    this.raio = raio;
    this.pwV = pw;
    this.classificacao = classi;
  }
  
  public Voluntario(Voluntario v){
   this.codVoluntario=v.getCodVoluntario();
   this.nomeV=v.getNomeV();
   this.gpsV=v.getGPSV();
   this.raio = v.getRaio();
   this.pwV = v.getPwV();
   this.classificacao = v.getClassificacao();
  }
  
  //Setters
  public String getCodVoluntario(){ 
      return this.codVoluntario;
    }
    
  public String getNomeV(){ 
      return this.nomeV;
    } 
    
  public GPS getGPSV(){ 
      return this.gpsV.clone();
    }   
    
  public double getRaio(){ 
      return this.raio;
    }
    
  public String getPwV(){
      return this.pwV;
    }   
   
  public int getClassificacao(){ 
    return this.classificacao;
    }
  //Setters  
  public void setCodVoluntario(String codigo){ 
    this.codVoluntario = codigo;
    }  
    
  public void setNomeV (String nome){ 
   this.nomeV = nome;
    }  
    
  public void setGpsV(GPS gps){ 
    this.gpsV = gps.clone();
    }
    
  public void setRaio(Double raio){ 
    this.raio = raio;
    }  
    
  public void setPwV(String pw){
      this.pwV = pw;
    }   
  
  public void setClassificacao(int classi){ 
    this.classificacao = classi;
    }
  //toString  
  public String toString(){
       StringBuilder sb = new StringBuilder();
       sb.append("Codigo: ").append(this.codVoluntario).append("\n").
       append("Nome: ").append(this.nomeV).append("\n").
       append("Local: ").append(this.gpsV).append("\n").
       append("Raio: ").append(this.raio).append("\n").
       append("Classificação: ").append(this.classificacao).append("\n");
       return sb.toString();
       //Nao mostra password
    }  
    
  //boolean  
  public boolean equals (Object o){ 
       if (o == this) return true;
      if (o == null || o.getClass() != this.getClass()) return false;
      Voluntario v = (Voluntario) o;
      return this.codVoluntario.equals(v.getCodVoluntario())&&
             this.nomeV.equals(v.getNomeV())&&
             this.gpsV.equals(v.getGPSV())&&
             this.raio == v.getRaio() &&
             this.pwV.equals(v.getPwV())&&
             this.classificacao == v.getClassificacao();
    } 
    
  //Clone  
  public Voluntario clone(){ 
    return new Voluntario(this);
    }
  

  //toStringCSV
  
  public String toStringCSV(){
       StringBuilder sb = new StringBuilder();
       sb.append("Voluntario:")
         .append(this.codVoluntario).append(",")
         .append(this.nomeV).append(",")
         .append(this.gpsV.getLongitude()).append(",")
         .append(this.gpsV.getLatitude()).append(",")
         .append(this.raio).append(",")
         .append(this.pwV).append(",")
         .append(this.classificacao);
       return sb.toString();  
  }
  
  public boolean inRangeV(Loja l,Utilizador u) throws NullPointerException {
        return (inRangeVtoL(l) && inRangeVtoU(u));
    }
  
  
  public boolean inRangeVtoL(Loja l){
     return (Math.sqrt(Math.pow(this.gpsV.getLongitude() + l.getGPSL().getLongitude(), 2) - Math.pow(this.gpsV.getLatitude() + l.getGPSL().getLatitude(), 2)) < this.raio); 
  }
  
  public boolean inRangeVtoU(Utilizador u){
      return (Math.sqrt(Math.pow(this.gpsV.getLongitude() + u.getGPS().getLongitude(), 2) - Math.pow(this.gpsV.getLatitude() + u.getGPS().getLatitude(), 2)) < this.raio);
    }    
}
