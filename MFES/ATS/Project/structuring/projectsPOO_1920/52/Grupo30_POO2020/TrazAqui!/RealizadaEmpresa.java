import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.*;
import java.io.*;


public class RealizadaEmpresa extends Encomenda implements Serializable
{
  private Empresa empresa;
  private double preco;
  private double distanciaViagem;
  private boolean classificado; // se ja foi classificado ou nao
  private double classificacao; // guarda-se a classificacao atribuida
  
  public RealizadaEmpresa(){
      super();
      this.empresa= new Empresa();
      this.preco=-1;
      this.distanciaViagem=-1;
      this.classificado=false;
      this.classificacao=-1;
    }
    
  public RealizadaEmpresa(String idAux, Cliente clienteAux, Loja lojaAux, double pesoAux, boolean stateAux,LocalDate dataAux,boolean respostaClienteAux, boolean flagLojaProntaAux, ArrayList<LinhaEncomenda> l, Empresa empresaAux, double precoAux,double distanciaViagemAux, boolean classificadoAux, double classificacaoAux){
      super(idAux, clienteAux, lojaAux, pesoAux, stateAux, dataAux,respostaClienteAux,flagLojaProntaAux,l);
      this.empresa= empresaAux;
      this.preco= precoAux;
      this.distanciaViagem= distanciaViagemAux;
      this.classificado=classificadoAux;
      this.classificacao=classificacaoAux;
    }
    
  public RealizadaEmpresa(RealizadaEmpresa e){
      super(e);
      this.empresa= e.getEmpresa();
      this.preco= e.getPreco();
      this.distanciaViagem= e.getDistanciaViagem();
      this.classificado=e.getClassificado();
      this.classificacao=e.getClassificacao();
    }
    
   //getters
   
   public Empresa getEmpresa(){
       return this.empresa;
    }
    
   public double getPreco(){
       return this.preco;
    }
    
  
    
   public double getDistanciaViagem(){
       return this.distanciaViagem;
    }
    
   public boolean getClassificado(){
       return this.classificado;
    }
    
   public double getClassificacao(){
        return this.classificacao;
    }
    
    // setters
    
    
   public void setEmpresa(Empresa e){
       this.empresa=e;
    }
    
   public void setPreco(double p){
       this.preco=p;
    }
 
     
   public void setDistanciaViagem(double d){
       this.distanciaViagem=d;
    }
    
   public void setClassificado(boolean c){
       this.classificado=c;
    }
    
   public void setClassificacao(double c){
       this.classificacao=c;
    }
    
   public boolean equals (Object o){
        if(this==o) return true;
        
        if((o==null) || (this.getClass()!=o.getClass())) return false;
        
        RealizadaEmpresa re= (RealizadaEmpresa) o;
        return(super.equals(re) && this.getEmpresa().equals(re.getEmpresa()) &&
               this.preco==re.getPreco() &&
               this.distanciaViagem==re.getDistanciaViagem() && this.classificado==re.getClassificado() &&
               this.classificado==re.getClassificado());
            }
            
            
   public String toString(){
       return "Encomenda com id: " + this.getId() +
               this.getCliente() +
               this.getLoja() +  "\n" +
              "Peso: " + this.getPeso() + " | " +
              " Entregue por Empresa: " + this.empresa.getNome() + "\n"+
              "Distancia da viagem: " + this.distanciaViagem + " | " +
              " Preco: " + this.preco + " | " +
              " Data de entrega: " + this.getData() +" | " +
              " Classificacao atribuida: " + this.classificacao +"\n\n";
            }
          
            
   public RealizadaEmpresa clone(){
       RealizadaEmpresa aux= new RealizadaEmpresa(this);
       aux.setData(this.getData());
        return aux;
    }
    
    //metodos
    public void clienteClassifica(double c){
        this.setClassificacao(c);
        this.setClassificado(true);
    }
}
