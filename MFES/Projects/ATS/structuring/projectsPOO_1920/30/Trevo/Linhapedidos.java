public class Linhapedidos{
 private String codproduto;
 private String descricao;
 private double quantidade;
 private double valorunitario;
 
 public Linhapedidos(){
 this.codproduto=new String();
 this.descricao=new String();
 this.quantidade=0;
 this.valorunitario=0.0;
 }
 
 public Linhapedidos(String codproduto, String descricao, double quantidade, double valorunitario){
 this.codproduto=codproduto;
 this.descricao=descricao;
 this.quantidade=quantidade;
 this.valorunitario=valorunitario;
 }
    
 public Linhapedidos(Linhapedidos e){
 this.codproduto=e.getcodproduto();
 this.descricao=e.getdescricao();
 this.quantidade=e.getquantidade();
 this.valorunitario=e.getvalorunitario();
 }
 
 public String getcodproduto(){
 return this.codproduto;    
 }
 
 public String getdescricao(){
 return this.descricao;    
 }
 
 public double getquantidade(){
 return this.quantidade;    
 }
 
 public double getvalorunitario(){
 return this.valorunitario;    
 }
 
 public void setcodproduto(String e){
 this.codproduto=e;    
 }
 
 public void setdescricao(String e){
 this.descricao=e;    
 }
 
 public void setquantidade(double e){
 this.quantidade=e;    
 } 
 
 public void setvalorunitario(double e){
 this.valorunitario=e;    
 }
 
 public Linhapedidos clone(){
 return new Linhapedidos(this);    
 }
 
 public String teste(){
 StringBuilder sb = new StringBuilder();
 sb.append("\n" + codproduto);
 return sb.toString();
 }
 
 public String nometocod(String e){
 return this.codproduto; 
 }
 
 public String toStringlindo(){
     StringBuilder sb = new StringBuilder();
     sb.append("Codigoproduto:" + codproduto + "\nDescricao:" + descricao + "\nQuantidade" + quantidade + "\nValor por unidade" + valorunitario);
     return sb.toString();
 }
 
 public String toString(){
     StringBuilder sb = new StringBuilder();
     sb.append(","+codproduto + "," + descricao + "," + quantidade + "," + valorunitario);
     return sb.toString();
 }
}