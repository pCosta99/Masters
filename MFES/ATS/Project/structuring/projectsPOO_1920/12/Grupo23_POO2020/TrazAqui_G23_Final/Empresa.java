import java.util.List;
import java.util.*; 
import java.nio.charset.StandardCharsets; 
import java.nio.file.*; 
import java.io.*;

/**
 * Subclasse Empresa :: Info adicional respetiva de cada empresa à 
 * entrega de uma encomenda. (Entregador Tipo 2)
 */
public class Empresa extends Utilizador implements Serializable
{
   private static double txpeso = 0.2;
   //taxa cobrada por transporte de encomenda, consoante o seu peso.
   
   private int NIF;
   private double raio; // raio de ação
   private double precoKM; //(em cêntimos) preço cobrado por Km percorrido numa Entrega.
                           //neste caso o preço varia de empresa para empresa
   private double nota; //nota dada pelo Comprador após Entrega
   private double numreviews; // nº de classificações totais que já lhe foram atríbuidas
   
   public Empresa(){
       super();
       this.raio = 0;
       this.NIF = 0;
       this.precoKM = 0;
       this.nota = 0;
       this.numreviews = 0;
   }
    
   public Empresa(Empresa e){
       super(e);
       this.raio=e.getRaio();
       this.NIF=e.getNIF();
       this.precoKM = e.getPrecoKM();
       this.nota = e.getNota();
       this.numreviews = e.getReviews();
       
   }
      
   public Empresa(String email, String pass, String name, Localizacao coord, int n, double r, double pkm, double nota, double nreviews){
       super(email,pass,name,coord);
       this.setRaio(r);
       this.setNIF(n);
       this.setPrecoKM(pkm);
       this.setNota(nota);
       this.setReviews(nreviews);
   }
   
   
   /**
    * Métodos get
    */
   public double getRaio(){
       return this.raio;
   }
   
   public int getNIF(){
       return this.NIF;
   }
   
   public double getPrecoKM(){
       return this.precoKM;
   }
   
   public double getNota(){
        return this.nota;
    }
    
   public double getReviews(){
        return this.numreviews;
   }

   /**
    * Métodos set
    */
   public void setRaio(double r){
       this.raio=r;
   }
   
   public void setNIF(int n){
       this.NIF = n;
   }
   
   public void setPrecoKM (double pkm){
       this.precoKM = pkm;
   }
   
   public void setNota(double n){
        this.nota = nota;
    }
   
   public void setReviews(double nreviews){
        this.numreviews = nreviews;
   }
   
   
   /**
    * Custo do transporte de uma encomenda.
    * Baseado na distância da loja ao comprador. Influenciado pelo peso 
    * da encomenda e respetiva taxa por peso cobrada pela empresa.
    */      
   public double custoTransE(Encomenda e, Localizacao loja, Localizacao comprador){
       double r = 0;
       double edistl = this.getCoord().distancia(loja);
       double ldistc = loja.distancia(comprador);
       r = e.getPeso()*this.txpeso + (edistl + ldistc)*this.precoKM; 
       
       return r*1.10; //(1.10 compensação pelo tempo de espera na loja)
    }
    
   
   /**
    * Clone
    */
   public Empresa clone(){
        return new Empresa(this);
   }
    
   /**
    * Equals
    */
   public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass()!=this.getClass()) return false;
        Empresa e = (Empresa) o;
        return (super.equals(e) &&
                this.raio==e.getRaio() &&
                this.NIF==e.getNIF() &&
                this.precoKM==e.getPrecoKM() &&
                this.numreviews==e.getReviews() &&
                this.nota==e.getNota());
    }
    
   /**
    * toString
    */
   public String toString(){
        return "Empresa /" + super.toString()
                           + " /NIF: " + this.getNIF()
                           + " /Raio: " + this.getRaio()
                           + " /Preco por Km: " + this.getPrecoKM()
                           + " /Classificacao: "+ this.getNota();
   }
   
   /**
    * toFile
    */
   public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("Transportadora:"+this.getEmail());
        sb.append(","+this.getPass());
        sb.append(","+this.getName());
        sb.append(","+this.getCoord().getLat());
        sb.append(","+this.getCoord().getLon());
        sb.append(","+this.getNIF());
        sb.append(","+this.getRaio());
        sb.append(","+this.getPrecoKM());
        sb.append(","+this.getNota());
        sb.append(","+this.getReviews());
       
        
        return(sb.toString());
    }
}