public class Queue{
 private String codigoProduto;
 private String desc;
 private double quantidade;
 private double valorPreco;
 
 //construtores
 public Queue(){
 this.codigoProduto=new String();
 this.desc=new String();
 this.quantidade=0;
 this.valorPreco=0.0;
 }
 
 public Queue(String codigoProduto, String desc, double quantidade, double valorPreco){
 this.codigoProduto=codigoProduto;
 this.desc=desc;
 this.quantidade=quantidade;
 this.valorPreco=valorPreco;
 }
    
 public Queue(Queue e){
 this.codigoProduto=e.getcodigoProduto();
 this.desc=e.getdesc();
 this.quantidade=e.getquantidade();
 this.valorPreco=e.getvalorPreco();
 }
 
 //getters e setters
 public String getcodigoProduto(){
 return this.codigoProduto;    
 }
 
 public String getdesc(){
 return this.desc;    
 }
 
 public double getquantidade(){
 return this.quantidade;    
 }
 
 public double getvalorPreco(){
 return this.valorPreco;    
 }
 
 public void setcodigoProduto(String e){
 this.codigoProduto=e;    
 }
 
 public void setdesc(String e){
 this.desc=e;    
 }
 
 public void setquantidade(double e){
 this.quantidade=e;    
 } 
 
 public void setvalorPreco(double e){
 this.valorPreco=e;    
 }
 
 public Queue clone(){
 return new Queue(this);    
 }
 
 public String teste(){
 StringBuilder sb = new StringBuilder();
 sb.append("\n" + codigoProduto);
 return sb.toString();
 }
 
 public String nameToCode(String e){         //dá o codigo do produto através do seu nome
 return this.codigoProduto; 
 }
 
 public String toString(){
     StringBuilder sb = new StringBuilder();
     sb.append(","+codigoProduto + "," + desc + "," + quantidade + "," + valorPreco);
     return sb.toString();
 }
}