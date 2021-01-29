import java.util.List;
import java.util.ArrayList;
import java.util.Iterator; 
import java.util.Random;

public class Pedido{
    private String codencomenda; 
    private String codut; 
    private String codloja;
    private double peso; 
    private List<Linhapedidos> linha; 
    
    public Pedido(){
    this.codencomenda = ""; 
    this.codut =""; 
    this.codloja ="";
    this.peso= 0.0; 
    this.linha=new ArrayList<Linhapedidos>();
    }
    public Pedido(String cod,String codut, String codloja, double peso,List<Linhapedidos>linha){
    this.codencomenda=cod;
    this.codut=codut; 
    this.codloja=codloja;
    this.peso=peso; 
    this.linha=new ArrayList<Linhapedidos>(linha);
    }
    public Pedido (Pedido e){
    this.codencomenda=e.getcodencomenda(); 
    this.codut=e.getcodut();
    this.codloja=e.getcodloja();
    this.peso=e.getPeso();
    this.linha=e.getLinha();
    }
    public String getcodencomenda(){
    return this.codencomenda; 
    }
  
    public String getcodut(){
    return this.codut;   
    }
    public String getcodloja(){
    return this.codloja;   
    }
    public double getPeso(){
    return this.peso;    
    }
    
    public List<Linhapedidos> getLinha(){
    return this.linha;
    }
    public void setcodencomenda(String ref){
    this.codencomenda=ref;
    }

    public void setcodut(String cod) {
    this.codut=cod; 
    }
    public void setcodloja(String cod){
    this.codloja=cod;
    }
    public void setpeso(double peso){
    this.peso=peso;   
    }
    public void setlinhaencomenda(List<Linhapedidos> e){
    this.linha=new ArrayList<Linhapedidos>(e);   
    }
   
    public Pedido clone(){
    return new Pedido (this); 
    }
    public boolean equals (Pedido o){
    return this.codencomenda==o.getcodencomenda();    
    }

    public double calculapreco(){
    double preco = 0.0; 
    for(Linhapedidos e : linha) 
    {
    preco+=e.getvalorunitario()*e.getquantidade();     
    }
        
    return preco;    
    }
    
    public List<String> codprod(){
    Iterator<Linhapedidos>it=linha.iterator();
    Linhapedidos e;
    List<String> cod = new ArrayList<>();
    while(it.hasNext()){
    e=it.next();
    cod.add(e.getcodproduto());
    }
    return cod; 
    }
    
    public boolean existepedido(String codprod, List<String> a){
    if (a.contains(codprod)) return true;
    return false;        
    }
    
    public void toStringlinhalinda(){
    for(Linhapedidos e : linha){
     System.out.println( e.toStringlindo());   
    }
        
    }
    
    public String toStringlinha(){
    Iterator<Linhapedidos>it=linha.iterator();
    Linhapedidos f;
    StringBuilder sb = new StringBuilder(); 
    
    while(it.hasNext()){
    f=it.next();  
    sb.append(f.toString());
    }
    return sb.toString(); 
    }
    
    public List<String> getcodprod(){
    List<String> ret = new ArrayList<>();
    for(Linhapedidos e : linha){
    ret.add(e.getcodproduto());
    }
    return ret;    
    }
    
    public String prods(){
    StringBuilder sb = new StringBuilder(); 
    for(Linhapedidos e : linha){
    sb.append("\n"+e.teste());    
    }
    return sb.toString();    
    }
   
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\n*********************Pedido*********************");
        sb.append("\nCodigo encomenda: " + codencomenda);
        sb.append("\nCodigo user: " + codut);
        sb.append("\nCodigo Loja: " + codloja);
        sb.append("\nPeso: " + peso);
        sb.append("\n********LinhaEncomenda********");
        sb.append(toStringlinha());
        sb.append("\n******************************");
        return sb.toString();  
    }
}