import java.util.List;
import java.util.ArrayList;
import java.util.*;
import java.io.*;


/**
 * Escreva a descrição da classe Cliente aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Cliente extends Utilizador implements Serializable
{
    private List<RealizadaEmpresa> re;
    private List<RealizadaVoluntario> rv;
    
   public Cliente(){
       super();
       this.re = new ArrayList<>();
       this.rv = new ArrayList<>();
    }
    
    public  Cliente(Cliente c){
        super(c);
        this.re=c.getRe();
        this.rv=c.getRv();
    }
    
    public Cliente(String email,String nome,String password,Localizacao localizacao,List<RealizadaEmpresa> reAux, List<RealizadaVoluntario> rvAux){
    super(email,nome,password,localizacao);
    this.re=reAux;
    this.rv=rvAux;
    
   }
   
    public List<RealizadaEmpresa> getRe(){
        List<RealizadaEmpresa> l = new ArrayList();
        for(RealizadaEmpresa r : this.re)
            l.add(r.clone());
        return l;
    }
    
    public List<RealizadaVoluntario> getRv(){
        List<RealizadaVoluntario> l = new ArrayList();
        for(RealizadaVoluntario r : this.rv)
            l.add(r.clone());
        return l;
    }
    
    public void setRe(List<RealizadaEmpresa> l){
        this.re = new ArrayList<>();
        for(RealizadaEmpresa r : l)
            this.re.add(r);
    }
    
    public void setRv(List<RealizadaVoluntario> l){
        this.rv = new ArrayList<>();
        for(RealizadaVoluntario r : l)
            this.rv.add(r);
    }
    
public boolean equals (Object o){
        if(this==o) return true;
        
        if((o==null) || (this.getClass()!=o.getClass())) return false;
        
        Cliente c = (Cliente) o;
        return(super.equals(c) && this.getRe().equals(c.getRe()) && this.getRv().equals(c.getRv()) );
    }
   
    
    public String toString() {
        String s = new String();
 
        s = ( "\nCliente->" +
                " Email: " + this.getEmail() + " | " +
                " Nome: " + this.getNome() + " | " +
                " Localizacao: "+ this.getLocalizacao());
                
                
 
        return s;
    }
    
    
    public Cliente clone() {
        return new Cliente(this);
    }
    
    //metodos para atualizar listas de encomendas entregues
    public void atualizaLE(RealizadaEmpresa r){
        ArrayList<RealizadaEmpresa> list = new ArrayList<RealizadaEmpresa>(this.getRe());
        list.add(r);
        this.setRe(list);
    }
    
    public void atualizaLV(RealizadaVoluntario r){
        ArrayList<RealizadaVoluntario> list = new ArrayList<RealizadaVoluntario>(this.getRv());
        list.add(r);
        this.setRv(list);
    }
}
