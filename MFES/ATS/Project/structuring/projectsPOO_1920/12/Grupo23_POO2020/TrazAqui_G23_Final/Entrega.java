import java.time.LocalDate;
import java.io.*;

/**
 * Classe Entrega :: Diz-nos informações gerais de uma dada entrega.
 * Informações do Comprador, Entregador(Loja ou Empresa) envolvidos, 
 * bem como Custo do processo, Data e se serviço já foi classificado por
 * Comprador ou não e respetiva avaliação atual.
 */
public class Entrega implements Serializable
{
    private static String nclass = "NAO CLASSIFICADA";
    private static String classificada = "CLASSIFICADA";
    
    private Localizacao cbuyer, cloja, centregador; //coords do comprador, loja e entregador
    private double custo;
    private String comprador;   //email buyer
    private String vendedor;    //email loja
    private String entregador; //email entregador
    private String idEncomenda;  //id encomenda
    private LocalDate data; //data da entrega
    private String avaliada;
    
    public Entrega(){
        this.cbuyer = new Localizacao();
        this.cloja = new Localizacao();
        this.centregador = new Localizacao();
        this.custo = 0;
        this.comprador = new String();
        this.vendedor = new String();
        this.entregador = new String();
        this.idEncomenda = new String();
        this.avaliada = nclass;
    }
    
    public Entrega(Localizacao cbuyer, Localizacao cloja, 
                    Localizacao centregador, double custo, String buyer,
                    String loja, String entregador, String idEncomenda, LocalDate data, String avaliada){
           this.cbuyer = cbuyer;
           this.cloja = cloja;
           this.centregador = centregador;
           this.custo = custo;
           this.comprador = buyer;
           this.vendedor = loja;
           this.entregador = entregador;
           this.idEncomenda = idEncomenda;
           this.data = data;
           this.avaliada = avaliada;
    }
    
    public Entrega(Entrega e){
        this.cbuyer = e.getCbuyer();
        this.cloja = e.getCloja();
        this.centregador = e.getCentregador();
        this.custo = e.getCusto();
        this.comprador = e.getComprador();
        this.vendedor = e.getVendedor();
        this.entregador = e.getEntregador();
        this.idEncomenda = e.getIDenc();
        this.data = e.getData();
        this.avaliada = e.getAvaliada();
    }
           
    /**
     * Métos get
     */
    
    public Localizacao getCbuyer(){
        return this.cbuyer;
    }
    
    public Localizacao getCloja(){
        return this.cloja;
    }
    
    public Localizacao getCentregador(){
        return this.centregador;
    }
    
    public double getCusto(){
        return this.custo;
    }
    
    public String getComprador(){
        return this.comprador;
    }
    
    public String getVendedor(){
        return this.vendedor;
    }
    
    public String getEntregador(){
        return this.entregador;
    }
    
    public String getIDenc(){
        return this.idEncomenda;
    }
    
    public LocalDate getData(){
        return this.data;
    }
    
     public String getAvaliada(){
        return this.avaliada;
    }
    
    
    /**
     * Métodos set
     */
    public void setCbuyer(Localizacao l){
        this.cbuyer = l;
    }
      
    public void setCloja(Localizacao l){
        this.cloja = l;
    }
      
    public void setCentregador(Localizacao l){
        this.centregador = l;
    }
       
    public void setCusto(double custo){
        this.custo = custo;
    }
    
    public void setComprador(String c){
        this.comprador = c;
    }
    
    public void setVendedor(String c){
        this.vendedor = c;
    }
    
    public void setEntregador(String c){
        this.entregador = c;
    }

    public void setIDenc(String c){
        this.idEncomenda = c;
    }
    
    public void setData(LocalDate d){
        this.data = d;
    }
    
    public void setAvaliada(String state){
        this.avaliada = state;
    }
        
    public void setClassificada(){
        this.avaliada = classificada;
    }
    
    public void setNClass(){
        this.avaliada = nclass;
    }
    
    /**
     * Clone
     */
    public Entrega clone(){
        return new Entrega(this);
    }
    
    /**
     * ToString
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Entrega - Comprador: "+this.getComprador()+
                    " / Loja: "+this.getVendedor()+
                    " / Custo: "+this.getCusto()+
                    " / Encomenda nº: "+this.getIDenc()+
                    " / Entregador: " + this.getEntregador()+
                    " / Data : "+this.getData() +
                    " / " + this.getAvaliada());
        return sb.toString();
    }
    
    /**
     * Equals
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass()!=this.getClass()) return false;
        Entrega e = (Entrega) o;
        return e.getCbuyer().equals(this.cbuyer) 
               && e.getCloja().equals(this.cloja)
               && e.getCentregador().equals(this.centregador)
               && e.getComprador().equals(this.comprador)
               && e.getVendedor().equals(this.vendedor)
               && e.getEntregador().equals(this.entregador)
               && e.getIDenc().equals(this.idEncomenda)
               && e.getCusto()==this.custo
               && e.getData().equals(this.data)
               && e.getAvaliada().equals(this.avaliada);
    }
    
    /**
     * toFile
     */
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("Entrega:"+this.getCbuyer().getLat());
        sb.append(","+this.getCbuyer().getLon());
        sb.append(","+this.getCloja().getLat());
        sb.append(","+this.getCloja().getLon());
        sb.append(","+this.getCentregador().getLat());
        sb.append(","+this.getCentregador().getLon());
        sb.append(","+this.getCusto());
        sb.append(","+this.getComprador());
        sb.append(","+this.getVendedor());
        sb.append(","+this.getEntregador());
        sb.append(","+this.getIDenc());
        sb.append(","+this.getData());
        sb.append(","+this.getAvaliada());
        
        return sb.toString();
    }
}
