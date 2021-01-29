import java.util.ArrayList;
import java.util.List; 


public class Accepted{         // depois de ter o código da encomenda coloca como aceite
 private List<String> codigoEncomenda; 
 
 //construtores
 public Accepted(){
 this.codigoEncomenda=new ArrayList<String>();    
 }
 
 public Accepted(List<String> codigoEncomenda){
 this.codigoEncomenda=new ArrayList<String> (codigoEncomenda);   
 }
 
 public Accepted(Accepted e){
 this.codigoEncomenda=e.getcodigoEncomenda();    
 }
 
 //getters e setters
 public List<String> getcodigoEncomenda(){
 return this.codigoEncomenda;    
 }
    
 public void setcodencomeda(List<String> e){
 this.codigoEncomenda=e;
 }
 
 
 public void addEncomenda(String e){
 this.codigoEncomenda.add(e);    
 }
 
 public void removerEncomenda(String e){
 this.codigoEncomenda.remove(e);    
 }
 
 

 public Accepted clone(){
 return new Accepted(this);    
 }
 
 public String toString(){
     StringBuilder sb = new StringBuilder();
     sb.append("\nPedidos aceites: " + codigoEncomenda);
     return sb.toString();      
 }
}