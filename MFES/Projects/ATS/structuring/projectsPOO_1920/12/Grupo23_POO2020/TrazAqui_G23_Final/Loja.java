import java.io.*;
import java.lang.String;
import java.util.ArrayList;
import java.lang.Object;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Subclasse Loja :: Info adicional respetiva de cada Loja à 
 * entrega de uma encomenda.
 */
public class Loja extends Utilizador implements Serializable
{
    // variáveis de instância 
    private boolean existefila; // valor que nos dirá se loja admite fila
                                //p.e. true: admite, false: não admite
                                //se admitir as outras funções em função dela irão aceder à fila respetiva
                                 
    private List<Encomenda> filaespera; //encomendas a sinalizar
    
    
    public Loja(){
        super();
        this.existefila = false;
        this.filaespera= new ArrayList<>();
    }
    
    public Loja (Loja l){
        super(l);
        this.existefila=l.getExiste();
        this.filaespera=l.getFila();
    }
    
    public Loja(String email, String pass, String name, Localizacao coord, boolean e, List <Encomenda> f){
       super(email,pass,name,coord);
       this.setExiste(e);
       this.setFila(f);
    }
    
    /**
     * Métodos get
     */
    public boolean getExiste(){
       return this.existefila;
    }
    
    public List<Encomenda> getFila(){
        List fila = new ArrayList<Encomenda>();
        for(Encomenda e: this.filaespera){
            fila.add(e.clone());
        }
        return fila;
    }
    
    /**
     * Métodos set
     */
    public void setExiste(boolean sn){
       this.existefila=sn;
    }
    
    public void setFila(List<Encomenda> f){
                         this.filaespera = f.stream().map(Encomenda::clone).collect(Collectors.toList());
    }
    
    public Loja clone(){
        return new Loja(this);
    }
    
    
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Loja l = (Loja) o;
        return (super.equals(l) &&
                this.existefila==l.getExiste() &&
                this.filaespera.equals(l.getFila()));
    }
    
    public String toString(){
        return "Loja /" + super.toString();
    }
    
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        sb.append("Loja:"+this.getEmail());
        sb.append(","+this.getPass());
        sb.append(","+this.getName());
        sb.append(","+this.getCoord().getLat());
        sb.append(","+this.getCoord().getLon());
        return(sb.toString());
    }
}
