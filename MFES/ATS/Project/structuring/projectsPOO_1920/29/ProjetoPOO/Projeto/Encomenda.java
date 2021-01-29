import java.util.*;
import java.util.ArrayList;
import java.io.Serializable;
public class Encomenda implements Serializable
{
    //Variaveis
    
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private ArrayList<LinhaEncomenda> linha;
    
    //Instancias
    public Encomenda(){
        this.codEncomenda = "n/a";
        this.codUtilizador = "n/a";
        this.codLoja = "n/a";
        this.peso = 0.0;
        this.linha = new ArrayList<LinhaEncomenda>();
    }    
    
    public Encomenda(String codenc, String codutil, String loja, Double pes, ArrayList<LinhaEncomenda> le){
        this.codEncomenda = codenc;
        this.codUtilizador = codutil;
        this.codLoja = loja;
        this.peso = pes;
        setLinha(le);
    }    
    
    public Encomenda (Encomenda e){
        this.codEncomenda = e.getCodEncomenda();
        this.codUtilizador = e.getCodUtilizador();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.linha = e.getLinha();
    }  
    
    //Getters
    
    public String getCodEncomenda(){
        return this.codEncomenda;    
    } 
    
    public String getCodUtilizador(){
        return this.codUtilizador;    
    } 
    
    public String getCodLoja(){
        return this.codLoja;    
    } 
    
    public Double getPeso(){
        return this.peso;
    } 
    
    public ArrayList<LinhaEncomenda> getLinha(){
        ArrayList<LinhaEncomenda> novo = new ArrayList<LinhaEncomenda>();
        for (LinhaEncomenda l: this.linha){
            novo.add(l.clone());
        }  
        return novo;
    }
    
    //Setters
    
    public void setCodEncomenda(String codenco){
        this.codEncomenda = codenco;    
    } 
    
    public void setCodUtilizador(String util){
        this.codUtilizador = util;
    }    
    
    public void setCodLoja(String codl){
        this.codLoja = codl;
    }    
    
    public void setPeso(Double peso){
        this.peso = peso;
    } 
    
    public void setLinha(ArrayList<LinhaEncomenda> le){
        this.linha = new ArrayList<>();
        for(LinhaEncomenda l: le){
            this.linha.add(l.clone());
        }    
    }
    

    //toString
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Encomenda: ").append(this.codEncomenda).append("\n").
           append("Codigo Utilizador: ").append(this.codUtilizador).append("\n").
           append("Codigo Loja: ").append(this.codLoja).append("\n").
           append("Peso: ").append(this.peso).append("\n").
           append("Linha Encomendada: ").append(this.linha).append("\n");
        return sb.toString();    
    }    
    
    //Equals
    
    public boolean equals (Object o){ 
      if (o == this) return true;
      if (o == null || o.getClass() != this.getClass()) return false;
      Encomenda e = (Encomenda) o;
      return this.codEncomenda.equals(e.getCodEncomenda())&&
             this.codUtilizador.equals(e.getCodUtilizador())&&
             this.codLoja.equals(e.getCodLoja())&&
             (this.peso == e.getPeso()) &&
             this.linha.equals(e.getLinha());
    } 
    
    //Clone
    
    public Encomenda clone(){
        return new Encomenda(this);
    }   
    
    
    //toStringCSV
    
    public String toStringCSV(){
       StringBuilder sb = new StringBuilder();
       sb.append("Encomenda:")
         .append(this.codEncomenda).append(",")
         .append(this.codUtilizador).append(",")
         .append(this.codLoja).append(",")
         .append(this.peso);
         for(LinhaEncomenda e: this.linha) {
             sb.append(",")
               .append(e.toStringCSV());
            }   
       return sb.toString();  
   }    
}
