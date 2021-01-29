import java.util.*; 
import java.nio.charset.StandardCharsets; 
import java.nio.file.*; 
import java.io.*;

/**
 * Subclasse Voluntários :: Info adicional respetiva de cada voluntário à 
 * entrega de uma encomenda. (Entregador Tipo 1)
 */
public class Voluntarios extends Utilizador implements Serializable
{
    private static double precoKM = 0.5;
    private static double taxaPeso = 0.1;
    //valores estáticos, previamente estabelecidos.
    //preço cobrado por Km efetuado numa entrega do voluntário assim como taxa
    //cobrada pelo peso da Encomenda. (Em cêntimos)
    
    private double raio; //raio de ação
    private boolean available; // disponibiliade para efetuar uma entrega
    private double nota; //nota dada pelo Comprador após Entrega
    private double numreviews; // nº de classificações totais que já lhe foram atríbuidas
    
    
    public Voluntarios(){
        super();
        this.raio = 0;
        this.available = true;
        this.nota = 0;
        this.numreviews = 0;
    }
    
    public Voluntarios(Voluntarios v){
        super(v);
        this.raio = v.getRaio();
        this.available = v.getAvailable();
        this.nota = v.getNota();
        this.numreviews = v.getReviews();
    }
    
    public Voluntarios(String email, String pass, String name, Localizacao coord, double raio,boolean ava, double nota, double nreviews){
        super(email,pass,name,coord);
        this.setRaio(raio);
        this.setAvailable(ava);
        this.setNota(nota);
        this.setReviews(nreviews);
    }
    
    
    /**
     * Métodos get
     */
    public double getRaio(){
        return this.raio;
    }

    public boolean getAvailable(){
        return this.available;
    }
    
    public double getNota(){
        return this.nota;
    }
    
    public double getReviews(){
        return this.numreviews;
    }
    
    public double getPrecoKM(){
       return this.precoKM;
    }
    
    /**
     * Métos set
     */
    public void setRaio(double raio){
        this.raio = raio;
    }

    public void setAvailable(boolean state){
        this.available = state;
    }
               
    public void setNota(double n){
        this.nota = n;
    }
   
    public void setReviews(double nreviews){
        this.numreviews = nreviews;
    }
    
    public void setPrecoKM (double pkm){
       this.precoKM = pkm;
    }
        
    
    /**
    * Custo do transporte de uma encomenda.
    * Baseado na distância da loja ao comprador. Influenciado pelo peso 
    * da encomenda e respetiva taxa por peso cobrada pelo voluntário.
    */
    public double custoTransV(Encomenda e, Localizacao loja, Localizacao comprador){
       double r = 0;
       double edistl = this.getCoord().distancia(loja);
       double ldistc = loja.distancia(comprador);
       r = e.getPeso()*this.taxaPeso + (edistl + ldistc)*this.precoKM;
       
       return r*1.10; //(1.10 compensação pelo tempo de espera na loja)
    }
    
    
    /**
     * Clone
     */
    public Voluntarios clone(){
        return new Voluntarios(this);
    }
    
    /**
     * Equals
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Voluntarios v = (Voluntarios) o;
        return (super.equals(v) &&
                this.raio==v.getRaio() &&
                this.available==v.getAvailable() &&
                this.numreviews==v.getReviews() &&
                this.nota==v.getNota());
    }
    
    /**
     * toString
     */
    public String toString(){
        return "Voluntario /" + super.toString() 
                              + " /Raio: " + this.getRaio()
                              + " /Disponivel: " + this.getAvailable()
                              + " /Preco por Km: " + this.getPrecoKM()
                              + " /Classificacao: " + this.getNota();
    }
    
    /**
     * toFile
     */
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("Voluntario:"+this.getEmail());
        sb.append(","+this.getPass());
        sb.append(","+this.getName());
        sb.append(","+this.getCoord().getLat());
        sb.append(","+this.getCoord().getLon());
        sb.append(","+this.getRaio());
        sb.append(","+this.getAvailable());
        sb.append(","+this.getNota());
        sb.append(","+this.getReviews());
        
        return(sb.toString());
    }
}