import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.*;
import java.io.*;

/**
 * Escreva a descrição da classe RealizadaVoluntario aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class RealizadaVoluntario extends Encomenda implements Serializable
{
    private Voluntario voluntario;
    private boolean classificado; // se ja foi classificado ou nao
    private double classificacao; // guarda-se a classificacao atribuida
    
    
    public RealizadaVoluntario(){
      super();
      this.voluntario= new Voluntario();
      this.classificado=false;
      this.classificacao=-1;
    }
    
    public RealizadaVoluntario(String idAux, Cliente clienteAux, Loja lojaAux, double pesoAux, boolean stateAux,LocalDate dataAux,boolean respostaClienteAux, boolean flagLojaProntaAux, ArrayList<LinhaEncomenda> l,Voluntario voluntarioAux,boolean classificadoAux, double classificacaoAux){
        super(idAux, clienteAux, lojaAux, pesoAux, stateAux, dataAux,respostaClienteAux,flagLojaProntaAux,l);
        this.voluntario=voluntarioAux;
        this.classificado=classificadoAux;
        this.classificacao=classificacaoAux;
    }
    
    public RealizadaVoluntario(RealizadaVoluntario v){
        super(v);
        this.voluntario=v.getVoluntario();
        this.classificado=v.getClassificado();
        this.classificacao=v.getClassificacao();
    }
    
    //getters
    
    public Voluntario getVoluntario(){
        return this.voluntario;
    }
    
   public boolean getClassificado(){
       return this.classificado;
    }
    
   public double getClassificacao(){
        return this.classificacao;
    }
    
    
   //setters
   
   public void setVoluntario(Voluntario v){
       this.voluntario=v;
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
        
        RealizadaVoluntario rv= (RealizadaVoluntario) o;
        return(super.equals(rv) && this.getVoluntario().equals(rv.getVoluntario()) &&
               this.classificado==rv.getClassificado() &&
               this.classificado==rv.getClassificado());
            }
            
   public String toString(){
       return "Encomenda com id: " + this.getId() +
               this.getCliente() +
               this.getLoja() + "\n" +
              "Peso:" + this.getPeso() + " | " +
              "Entregue por Voluntario:" + this.voluntario.getEmail() +" | " +
              " Data de entrega:" + this.getData() + " | " +
              " Classificacao atribuida:" + this.classificacao +"\n\n";
            }
            
    public RealizadaVoluntario clone(){
        RealizadaVoluntario aux= new RealizadaVoluntario(this);
       aux.setData(this.getData());
        return aux;
    }
    
    //metodos
    public void clienteClassifica(double c){
        this.setClassificacao(c);
        this.setClassificado(true);
    }
}
