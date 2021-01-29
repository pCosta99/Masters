import java.util.List;
import java.util.ArrayList;
import java.util.Iterator; 
import java.util.Random;

public class Pedido{
    private String codigoEncomenda; 
    private String codigoUsuario; 
    private String codigoLoja;
    private double peso; 
    private List<Queue> linha; 
    
    //construtores
    public Pedido(){
    this.codigoEncomenda = ""; 
    this.codigoUsuario =""; 
    this.codigoLoja ="";
    this.peso= 0.0; 
    this.linha=new ArrayList<Queue>();
    }
    
    public Pedido(String cod,String codigoUsuario, String codigoLoja, double peso,List<Queue>linha){
    this.codigoEncomenda=cod;
    this.codigoUsuario=codigoUsuario; 
    this.codigoLoja=codigoLoja;
    this.peso=peso; 
    this.linha=new ArrayList<Queue>(linha);
    }
    
    public Pedido (Pedido e){
    this.codigoEncomenda=e.getcodigoEncomenda(); 
    this.codigoUsuario=e.getcodigoUsuario();
    this.codigoLoja=e.getcodigoLoja();
    this.peso=e.getPeso();
    this.linha=e.getLinha();
    }
    
    //getters e setters
    public String getcodigoEncomenda(){
    return this.codigoEncomenda; 
    }
  
    public String getcodigoUsuario(){
    return this.codigoUsuario;   
    }
    
    public String getcodigoLoja(){
    return this.codigoLoja;   
    }
    
    public double getPeso(){
    return this.peso;    
    }
    
    public List<Queue> getLinha(){
    return this.linha;
    }
    
    public void setcodigoEncomenda(String ref){
    this.codigoEncomenda=ref;
    }

    public void setcodigoUsuario(String cod) {
    this.codigoUsuario=cod; 
    }
    
    public void setcodigoLoja(String cod){
    this.codigoLoja=cod;
    }
    
    public void setpeso(double peso){
    this.peso=peso;   
    }
    
    public void setlinhaencomenda(List<Queue> e){
    this.linha=new ArrayList<Queue>(e);   
    }
   
    public Pedido clone(){
    return new Pedido (this); 
    }
    
    public boolean equals (Pedido o){
    return this.codigoEncomenda==o.getcodigoEncomenda();    
    }

    public double calculapreco(){ //calcula o preco do pedido
    double preco = 0.0; 
    for(Queue e : linha) 
    {
    preco+=e.getvalorPreco()*e.getquantidade();     
    }
        
    return preco;    
    }
    
    public List<String> codprod(){ //le o codigo do produto
    Iterator<Queue>it=linha.iterator();
    Queue e;
    List<String> cod = new ArrayList<>();
    while(it.hasNext()){
    e=it.next();
    cod.add(e.getcodigoProduto());
    }
    return cod; 
    }
    
    public boolean haPedido(String codprod, List<String> a){ // ve se ja existe esse pedido
    if (a.contains(codprod)) return true;
    return false;        
    }
    
    public void quantidade(String codprod,int e){    
    Iterator<Queue>it=linha.iterator();
    Queue f; 
    
    while(it.hasNext()){
    f=it.next();
    if(codprod.equals(f.getcodigoProduto())){
    f.setquantidade(e);
    }
    else {
    f=it.next();   
    }
    }
        
    }
    
    public String toStringlinha(){ //to string especializado
    Iterator<Queue>it=linha.iterator();
    Queue f;
    StringBuilder sb = new StringBuilder(); 
    
    while(it.hasNext()){
    f=it.next();  
    sb.append(f.toString());
    }
    return sb.toString(); 
    }
    
    public List<String> getcodprod(){        //mostra códigos de produto feitos na encomenda
    List<String> ret = new ArrayList<>();
    for(Queue e : linha){
    ret.add(e.getcodigoProduto());
    }
    return ret;    
    }
    
    public String produtos(){             //mostra todos os produtos que há nessa encomenda
    StringBuilder sb = new StringBuilder(); 
    for(Queue e : linha){
    sb.append("\n"+e.teste());    
    }
    return sb.toString();    
    }
   
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\n*********************Pedido*********************");
        sb.append("\nCodigo encomenda: " + codigoEncomenda);
        sb.append("\nCodigo user: " + codigoUsuario);
        sb.append("\nCodigo Loja: " + codigoLoja);
        sb.append("\nPeso: " + peso);
        sb.append("\n********LinhaEncomenda********");
        sb.append(toStringlinha());
        sb.append("\n******************************");
        return sb.toString();  
    }
}