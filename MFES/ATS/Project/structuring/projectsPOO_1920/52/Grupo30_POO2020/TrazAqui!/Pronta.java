import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.*;
import java.io.*;

/**
 * Escreva a descri��o da classe Pronta aqui.
 * 
 * @author (seu nome) 
 * @version (n�mero de vers�o ou data)
 */
public class Pronta extends Encomenda implements Serializable
{
    // vari�veis de inst�ncia - substitua o exemplo abaixo pelo seu pr�prio
    private Empresa empresa;
    private double preco;
    
    public Pronta(){
        super();
        this.empresa= new Empresa();
        this.preco=-1;
    }
    
    public Pronta(String idAux, Cliente clienteAux, Loja lojaAux, double pesoAux, boolean stateAux,LocalDate dataAux, boolean respostaClienteAux, boolean flagLojaProntaAux,ArrayList<LinhaEncomenda> l,Empresa empresaAux, double precoAux){
        super(idAux, clienteAux, lojaAux, pesoAux, stateAux, dataAux,respostaClienteAux,flagLojaProntaAux,l);
        this.empresa= empresaAux;
        this.preco=precoAux;
    }
    
    public Pronta(Pronta p){
        super(p);
        this.empresa= p.getEmpresa();
        this.preco=p.getPreco();
    }
    
    //getters
    
    public Empresa getEmpresa(){
       return this.empresa;
    }
    
    public double getPreco(){
        return this.preco;
    }
    
  public void setEmpresa(Empresa e){
       this.empresa=e;
    }
    
    public void setPreco(double preco){
        this.preco=preco;
    }
    public boolean equals (Object o){
        if(this==o) return true;
        
        if((o==null) || (this.getClass()!=o.getClass())) return false;
        Pronta p=(Pronta) o;
        return (super.equals(p) && this.getEmpresa().equals(p.getEmpresa()) && this.preco==p.getPreco());
    }
    
    public String toString(){
       return "Encomenda com id: " + this.getId() +
               this.getCliente() +
               this.getLoja() + "\n" +
              "Peso:" + this.getPeso() + "\n" +
              "Empresa que prentende transportar:"+ this.empresa.getEmail() + "\n" + 
              "Com preco:" + this.preco + "\n\n";
              
            }
            
            
    public Pronta clone(){
        return new Pronta(this);
    }
    
}
