import java.util.*;
import java.io.Serializable;
public class Transportadora implements Serializable
{
    
   //Variaveis 
   private String codEmpresa;
   private String nomeE;
   private GPS gpsE;
   private String nif;
   private double raioE;
   private double precokm;
   private String pwT;
   private int classificacao;
   private double faturado;
   private double km;
   
   //Instancias
   public Transportadora(){
    this.codEmpresa= "n/a";
    this.nomeE = "n/a";
    this.gpsE = new GPS();
    this.nif = "n/a";
    this.raioE=0.0;
    this.precokm = 0.0;
    this.pwT = "0000";
    this.classificacao = 0;
    this.faturado = 0;
    this.km =0;
    }
    
   public Transportadora(String codigo,String nome,GPS gps,String nif,double raio,double precokm, String pw,int classi, double fat,double km){ 
    this.codEmpresa= codigo;
    this.nomeE = nome;
    this.gpsE = gps.clone();
    this.nif = nif;
    this.raioE= raio;
    this.precokm = precokm;
    this.pwT = pw;
    this.classificacao = classi;
    this.faturado = fat;
    this.km = km;
    }
    
   public Transportadora(Transportadora t){
    this.codEmpresa= t.getCodEmpresa();
    this.nomeE = t.getNomeE();
    this.gpsE = t.getGPSE();
    this.nif = t.getNif();
    this.precokm = t.getPrecoKm();
    this.raioE = t.getRaioE();
    this.pwT = t.getPwT();
    this.classificacao = t.getClassificacao();
    this.faturado = t.getFaturado();
    this.km = t.getKm();
    }
    
   //Getters
   public Double getKm(){
      return this.km;
    }
   public String getCodEmpresa(){ 
    return this.codEmpresa;
    }
    
   public String getNomeE(){ 
    return this.nomeE;
    }
    
   public GPS getGPSE(){ 
    return this.gpsE.clone();
    }
    
   public String getNif(){ 
    return this.nif;
    }
    
   public double getPrecoKm(){
    return this.precokm;
    }
    
   public double getRaioE(){ 
    return this.raioE;
    }
    
   public String getPwT(){
       return this.pwT;
    } 
   
   public int getClassificacao(){ 
    return this.classificacao;
    }
    
   public double getFaturado(){
       return this.faturado;
    }    
   //Setters 
   public void setCodEmpresa(String codigo){ 
    this.codEmpresa = codigo;
    }
    
   public void setNomeE(String nome){ 
    this.nomeE = nome;
    }
    
   public void setGPSE(GPS gps){ 
    this.gpsE = gps.clone();
    }
    
   public void setNif(String nif){ 
    this.nif = nif;
    }
    
   public void setPrecoKm(Double preco){ 
    this.precokm = preco;
    }
    
   public void setRaioE(Double raio){ 
    this.raioE = raio ;
    }
    
   public void setPwT(String pw){
       this.pwT = pw;
    }   
    
   
   public void setClassificacao(int classi){ 
     this.classificacao = classi;
    }
   
    //toString 
   public String toString(){
       StringBuilder sb = new StringBuilder();
       sb.append("Codigo: ").append(this.codEmpresa).append("\n").
       append("Nome: ").append(this.nomeE).append("\n").
       append("Local: ").append(this.gpsE).append("\n").
       append("Nif: ").append(this.nif).append("\n").
       append("Preço(p/km): ").append(this.precokm).append("\n").
       append("Raio: ").append(this.raioE).append("\n").
       append("Classificação: ").append(this.classificacao).append("\n").
       append("Faturado: ").append(this.faturado).append("\n").
       append("Kilometros: ").append(this.km);
       return sb.toString();
       //Nao mostra password
    }   
    
   //Boolean 
   public boolean equals (Object o){ 
       if (o == this) return true;
      if (o == null || o.getClass() != this.getClass()) return false;
      Transportadora t = (Transportadora) o;
      return this.codEmpresa.equals(t.getCodEmpresa())&&
             this.nomeE.equals(t.getNomeE())&&
             this.gpsE.equals(t.getGPSE())&&
             this.raioE == t.getRaioE() &&
             this.nif.equals(t.getNif()) &&
             this.precokm == t.getPrecoKm() &&
             this.pwT.equals(t.getPwT())&&
             this.classificacao == t.getClassificacao();
    } 
    
   //Clone 
   public Transportadora clone (){ 
    return new Transportadora(this);
    }
    
   public void somaFat(double fati){
       this.faturado += fati;
    }    
    
   public void somakm(double km){ 
    this.km += km;
    }
    
   public String toStringCSV(){
       StringBuilder sb = new StringBuilder();
       sb.append("Transportadora:")
         .append(this.codEmpresa).append(",")
         .append(this.nomeE).append(",")
         .append(this.gpsE.getLongitude()).append(",")
         .append(this.gpsE.getLatitude()).append(",")
         .append(this.nif).append(",")
         .append(this.raioE).append(",")
         .append(this.precokm).append(",")
         .append(this.pwT).append(",")
         .append(this.classificacao).append(",")
         .append(this.faturado).append(",")
         .append(this.km);
       return sb.toString();  
   }     
   
   public boolean inRangeT(Loja l,Utilizador u) throws NullPointerException {
        return (inRangeTtoL(l) && inRangeTtoU(u));
    }
  
  
  public boolean inRangeTtoL(Loja l){
     return (Math.sqrt(Math.pow(this.gpsE.getLongitude() + l.getGPSL().getLongitude(), 2) - Math.pow(this.gpsE.getLatitude() + l.getGPSL().getLatitude(), 2)) < this.raioE); 
  }
  
  public boolean inRangeTtoU(Utilizador u){
      return (Math.sqrt(Math.pow(this.gpsE.getLongitude() + u.getGPS().getLongitude(), 2) - Math.pow(this.gpsE.getLatitude() + u.getGPS().getLatitude(), 2)) < this.raioE);
    }
   
  public double inRangeTtoLkm(Loja l){
     return Math.sqrt(Math.pow(this.gpsE.getLongitude() + l.getGPSL().getLongitude(), 2) - Math.pow(this.gpsE.getLatitude() + l.getGPSL().getLatitude(), 2)); 
  }
  
  
}