import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.List;
import java.util.ArrayList;
import java.lang.Object;
import java.time.DateTimeException;
import java.time.LocalDate;

public class Transportadora extends User
{
    private double custoKM;
    private double raio ; 
    private int nif; 
    private static double taxapeso = 0.5;
    private double avaliacao;
    private double numreviews;
    
/*
    private ArrayList<Encomenda> encomenda;
    private ArrayList<Encomenda> historico; */
    
    
    /**
    * COnstrutor para objetos da classe Transportadora
     */
    public Transportadora()
    {
        super();
        this.custoKM = 0;
        this.raio=0;
        this.nif=0;
        this.avaliacao=0;
        this.numreviews=0;
        
        
    }
    
    public Transportadora( String username , String codigo , String password , double locationX,
    double locationY,String email, double custoKM, double raio , int nif , double aval , double numrev){
            super(username,codigo,password,locationX,locationY,email);
            this.custoKM = custoKM;
            this.raio=raio;
            this.nif=nif;
            this.avaliacao=aval;
            this.numreviews=numrev;
    }

    public Transportadora(Transportadora p){
            super(p.getUserName(),p.getPassword(),p.getCodigo(),p.getLocationX(),p.getLocationY(),p.getEmail());
            this.custoKM = p.getcustoKM();
            this.raio=p.getraio();
            this.nif=p.getnif();
            this.numreviews = p.getnumreviews();
            this.avaliacao = p.getavaliacao();
            

    }
    
    public void setnumreviews (double  x){
        this.numreviews = x;
    }
    
    public double getnumreviews(){
           return this.numreviews;
    }
    
    public double gettaxapeso(){
           return this.taxapeso;
    }
    
    
    public void setavaliacao(double x){
           this.avaliacao=x;
    }
    
    public double getavaliacao(){
           return this.numreviews;
    }
    
    public double getcustoKM(){
           return this.custoKM;
    }
    
    public void setcustoKM(double custoKM){
           this.custoKM=custoKM;
    }
    
    public double getraio(){
           return this.raio;
    }
    
    public void setraio(int raio){
           this.raio=raio;
    }
    
    public int getnif(){
           return this.nif;
    }
    
    public void setnif(int nif){
           this.nif=nif;
    }
 
    public boolean equals(Object o){
        if(o==this) return true;
        if(o== null || o.getClass()!=this.getClass()) return false;
        Transportadora c= (Transportadora) o;
        return super.equals(c);
    }
    
    
    public String toString(){
        StringBuilder res = new StringBuilder();
        res.append(super.toString());
        res.append(this.custoKM);
        res.append(this.raio);
        res.append(this.nif);
        res.append(this.avaliacao);
        res.append(this.numreviews);
        return res.toString();
    }
    

    
    
    public Transportadora clone(){
        return new Transportadora(this);
    }
    
    
    
    
    /*
    public void PrintHist(){         
        for(Encomenda a : this.historico){
            String buyer= a.getBuyer().getUserName();
            String store=a.getLoja().getUserName();
            System.out.println("Buyer: " + buyer +",Store: " + store );
        }            
    }
    
    public double KmsPercurridos(){
        double totalKms =0;
        for(Encomenda a : this.historico){
            Buyer b = a.getBuyer();
            Loja l = a.getLoja(); 
            double transtoloja = Math.sqrt(Math.pow(this.locationX - l.getLocationX(), 2) +
                     Math.pow(this.locationY - l.getLocationY(), 2));
            double lojatoBuyer = Math.sqrt(Math.pow(l.getLocationX() - b.getLocationX(), 2) +
                     Math.pow(l.getLocationY() - b.getLocationY(), 2));
            totalKms= totalKms + lojatoBuyer+transtoloja;
        }
        return totalKms; 
    }
    //---------------------------------------------------
    public double faturacao(LocalDate d1, LocalDate d2){
        double total=0;
        for(Encomenda a : this.historico){
          if(a.getData().isAfter(d1) && a.getData().isBefore(d2) ){
              //total = inserir preço da encomenda aqui ------------------------------
            }
        }
        return total;
    }*/
    
    
        public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("Voluntario:"+this.getCodigo());
        sb.append(","+this.getPassword());
        sb.append(","+this.getUserName());
        sb.append(","+this.getLocationX());
        sb.append(","+this.getLocationY());
        sb.append(","+this.getEmail());
        sb.append(","+this.getraio());
        sb.append(","+this.getcustoKM());
        sb.append(","+this.getnumreviews());
        sb.append(","+this.getavaliacao());
        
        
        return(sb.toString());
    }

}