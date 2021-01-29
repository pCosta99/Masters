import java.text.DecimalFormat;
import java.util.Random;
import java.io.Serializable;
/**
 * Escreva a descrição da classe Transporte aqui.
 * 
/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Transporte implements Serializable{
    
    // Variaveis de instancia
    private Localizacao locInicio;
    private Localizacao locFim;
    
    private String utilizador; //email do cliente
    private String transportador; //email de quem transporta
    private String loja; // email da loja da encomenda
    private String codigo; // codigo da encomenda
   
    private double distUtilizador;//distancia ao utilizador
    
    private double precoEstimado;//preco estimado da entrega
    private double precoReal;//preco real da entrega
    
    private double tempoEstimado;//tempo estimado da entrega
    private double tempoReal;//tempo real da estrega
    
    private int classificacao;//classificaçao dada pelo utilizador
   
    /**
     * Construtor de Transporte sem parametros
     */
    public Transporte(){
        locInicio = new Localizacao();
        locFim = new Localizacao();
        
        utilizador = "";
        transportador="";
        loja="";
        codigo="";
        
        precoEstimado = 0.0;
        precoReal= 0.0;
        
        distUtilizador = 0.0;
        
        tempoEstimado=0.0;
        tempoReal=0.0;
        
        classificacao=0;
    }
    
    /** 
     * Construtor parametrizado 
     */
    public Transporte(Localizacao locInicio, Localizacao locFim,String utilizador,String transportador,String loja,String codigo, 
    double precoEstimado,double precoReal,double distUtilizador, double tempoEstimado, double tempoReal){
        this.locInicio = locInicio;
        this.locFim = locFim;
        
        this.utilizador = utilizador;
        this.transportador = transportador;
        this.loja = loja;
        this.codigo=codigo;
        
        this.precoEstimado = 0;
        this.precoReal = 0;
        
        this.tempoEstimado=tempoEstimado;
        this.tempoReal=tempoReal;
        
        this.distUtilizador = distUtilizador; 
        this.classificacao=0;
    } 
    
    /**
     * Construtor de copia
     */
    public Transporte(Transporte t){
        locInicio= t.getLocInicio();
        locFim = t.getLocFim();
        
        utilizador = t.getUtilizador();
        transportador = t.getTransportador();
        loja = t.getLoja();
        codigo=t.getCodigo();
        
        tempoEstimado=t.getTempoEstimado();
        tempoReal=t.getTempoReal();
        
        precoEstimado = t.getPrecoEstimado();
        precoReal = t.getPrecoReal();
        
        distUtilizador = t.getDistUtilizador();
        classificacao = t.getClassificacao();
    }
    

  
    /**
     * Get da localizaçao do transporte no eixo do x 
     */
    public Localizacao getLocInicio(){
        return locInicio;
    }
    
    /**
     * Get da localizaçao do transporte no eixo do y
     */
    public Localizacao getLocFim(){
        return locFim;
    }
    
    public String getUtilizador(){
        return this.utilizador;
    }
    
    public String getTransportador(){
        return this.transportador;
    }
    
    public String getLoja(){
        return this.loja;
    }
    
    public String getCodigo(){
        return this.codigo;
    }
    
    public double getTempoEstimado(){
        return this.tempoEstimado;
    }
    
    public double getTempoReal(){
        return this.tempoReal;
    }
    
    public double getPrecoEstimado(){
        return this.precoEstimado;
    }
    
    public double getPrecoReal(){
        return this.precoReal;
    }
    
    
    /**
     * Get da distancia ao Utilizador
     */
    public double getDistUtilizador(){
        return this.distUtilizador;
    }
    
    public int getClassificacao(){
        return this.classificacao;
    }
    
    public void setUtilizador(String u){
        utilizador = u;
    }
    
    
    public void setTransportador(String t){
        transportador = t;
    }
    
    public void setCodigo(String c){
        codigo= c;
    }
    
    public void setLoja(String l){
        loja = l;
    }
    
    public void setTempoEstimado(double te){
        tempoEstimado=te;
    }
    
    public void setTempoReal(double t){
        tempoReal=t;
    }
    
    public void setPrecoEstimado(double pe){
        precoEstimado=pe;
    }
    
    public void setPrecoReal(double pr){
        precoReal=pr;
    }
    
    /**
     * Altera a distancia ao utilizador
     */
    public void setDistLocalizaçao(double d){
        distUtilizador = d;
    }
    
    public void setClassificacao(int c){
        classificacao = c;
    }
    
     DecimalFormat df = new DecimalFormat("#.0");
    /**
     * Implementaçao do metodo toString
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Transporte:\n");
        sb.append("A localizaçao de recolha da encomenda: "+locInicio+"\n");
        sb.append("A localizaçao de entrega da encomenda: "+locFim+"\n");
        sb.append("O cliente: "+ utilizador+"\n");
        sb.append("Quem transporta: " + transportador+"\n");
        sb.append("De que loja: " + loja+"\n");
        sb.append("Código da encomenda: " + codigo + "\n");
        sb.append("Tempo estimado de entrega: " + df.format(tempoEstimado)+ " minutos\n");
        sb.append("Preço estimado da entrega: "+df.format(precoEstimado)+" euros\n");
        sb.append("Tempo real de entrega: " + df.format(tempoReal)+" minutos\n");
        sb.append("Preço real da entrega: " + df.format(precoReal)+" euros\n");
        sb.append("A distancia ao utilizador: "+df.format(distUtilizador)+" kms\n");
        sb.append("A classificação do transporte: " + classificacao+"\n");
        return sb.toString();
    }
    
    /**
     * Metodo equals
     * compara um objeto para ver se e um transporte
     */
    public boolean equals(Object o){
        if(this == o) return true;
        if(this.getClass() != o.getClass()) return false;
        Transporte a = (Transporte)o;

        if(this.locInicio != a.getLocInicio()) return false;
        if(this.locFim != a.getLocFim()) return false;
        if(this.utilizador != a.getUtilizador()) return false;
        if(this.transportador != a.getTransportador()) return false;
        if(this.loja != a.getLoja()) return false;
        if(this.codigo != a.getCodigo()) return false;
        if(this.precoEstimado != a.getPrecoEstimado()) return false;
        if(this.precoReal != a.getPrecoReal()) return false;
        if(this.classificacao != a.getClassificacao()) return false;
        return true;
    }
    
    /**
     * Metodo clone que faz uma copia do objeto Transporte
     */
    public Transporte clone(){
        return new Transporte(this);
    }
    
    //faz o resumo do transporte , definir o que por!!
    public String resumo(){
        StringBuilder sb = new StringBuilder();
        
        //informaçao sobre o encomendas transportadas
        
        
        return sb.toString();
    }
    
    
     public static void antesTransporteT(Utilizador u,Transportadora  t,Loja l, Encomenda e,Localizacao x, DataBase db) {
        double dist = l.getPosicao().distance(x);
        
        Transporte trans = new Transporte(l.getPosicao(),x,u.getTag(),t.getTag(),l.getTag(),e.getCodEncomenda(),0.0,0.0,dist,0.0,0.0);
        
        //transportadora faz o orçamento 
        t.fazOrçamento(trans,db);
    }
    
    /** 
     * o Utilizador escolheu uma transportadora que vai fazer o transporte da encomenda da loja para a sua casa
     */
    public static void fazTransporteT(Utilizador u,Transportadora  t,Loja l, Encomenda e,Localizacao x, DataBase db) throws UserNaoExistenteException {
        double dist = l.getPosicao().distance(x);
        
        Transporte trans = new Transporte(l.getPosicao(),x,u.getTag(),t.getTag(),l.getTag(),e.getCodEncomenda(),0.0,0.0,dist,0.0,0.0);
        
        //transportadora faz o orçamento 
        t.aceitaOrçamento(trans,db);
        
        // altera os valores estimados com os reais depois do transporte
        alteraTransporteFinal(trans,db);
        
        // adiciona o transporte da encomenda feito a cada um dos intervenientes
        int index = db.add(trans);
        //conta mais um transporte de encomendas aos transportes totais
        u.addTransporte(index); 
        t.addTransporte(index,db);
        l.addTransporte(index,db);
    }
   
    
     /** 
     * o Utilizador escolheu um voluntario que vai fazer o transporte da encomenda da loja para a sua casa
     */
    
    public static void fazTransporteV(Utilizador u,Voluntario v,Loja l, Encomenda e,Localizacao x, DataBase db) throws UserNaoExistenteException{
       double dist = l.getPosicao().distance(x);
  
        Transporte trans = new Transporte(l.getPosicao(),x,u.getTag(),v.getTag(),l.getTag(),e.getCodEncomenda(),0.0,0.0,dist,0.0,0.0);
        
        //volunario faz estimativa de tempo 
        v.fazEstimativa(trans,db);
        
        // altera os valores estimados com os reais depois do transporte
        alteraTransporteFinal(trans,db);
        
        // adiciona o transporte da encomenda feito a cada um dos intervenientes
        int index = db.add(trans);
        //conta mais um transporte de encomendas aos transportes totais
        u.addTransporte(index); 
        v.addTransporte(index,db);
        l.addTransporte(index,db);
       }
    
    
    
    private static void alteraTransporteFinal(Transporte t, DataBase db){
        
        //o tempo real vai ser o tempo estimado vezes uma percentagem de 75% a 125% caso ele demore mais ou menos na entrega 
        t.tempoReal = t.tempoEstimado * geraPercRandom();
        t.precoReal = (t.precoEstimado * t.tempoReal)/t.tempoEstimado;
    }
    
    // gera uma percentagem random entre 75% a 125%
    private static double geraPercRandom(){
        double x;
        do{
            Random rn = new Random();
            x = rn.nextInt() % 51;
        }while(x<= 0);
        return x/100 + 0.75;
    }
}
