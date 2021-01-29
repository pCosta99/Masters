import java.util.ArrayList;
import java.util.List; 


public class Pedidosaceites{
 private List<String> codencomenda; 
 
 public Pedidosaceites(){
 this.codencomenda=new ArrayList<String>();    
 }
 
 public Pedidosaceites(List<String> codencomenda){
 this.codencomenda=new ArrayList<String> (codencomenda);   
 }
 
 public Pedidosaceites(Pedidosaceites e){
 this.codencomenda=e.getcodencomenda();    
 }
 
 public List<String> getcodencomenda(){
 return this.codencomenda;    
 }
    
 public void setcodencomeda(List<String> e){
 this.codencomenda=e;
 }
 
 public void addcodencomenda(String e){
 this.codencomenda.add(e);    
 }
 
 public void removecodencomeda(String e){
 this.codencomenda.remove(e);    
 }
 
 public Pedidosaceites clone(){
 return new Pedidosaceites(this);    
 }
 
 public String pedidos(){
   StringBuilder sb = new StringBuilder();
   for(String e : codencomenda){
   sb.append(codencomenda);
   }
     return sb.toString();    
    }
 
 public String toString(){
     StringBuilder sb = new StringBuilder();
     for(String e : codencomenda){
     sb.append("\nAceite: " + codencomenda);
    }
     return sb.toString();      
 }
}