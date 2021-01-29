import java.io.*;
import java.lang.String;
import java.util.ArrayList;
import java.lang.Object;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Subclasse Comprador :: Utilizador standart da aplicação, que efetua as 
 * encomendas a uma dada Loja/Empresa.
 */
public class Comprador extends Utilizador implements Serializable
{
    List<Encomenda> encomendas;
    
    public Comprador(){
        super();
        this.encomendas = new ArrayList<Encomenda>();
    }
    
    public Comprador(Comprador c){
        super(c);
        this.encomendas = c.getEncomenda();
    }
    
    public Comprador(String email, String pass, String name, Localizacao coord, List<Encomenda> e){
        super(email,pass,name,coord);
        this.setEncomenda(e);
    }
    
    
    /**
     * Métodos get e set
     */
    public List<Encomenda> getEncomenda(){
        List<Encomenda> copia = new ArrayList<>();
        for (Encomenda e : this.encomendas){
            copia.add(e.clone());
        }
        return copia;
    }
    
    public void setEncomenda(List<Encomenda> enc){
        this.encomendas = new ArrayList<>();
        for (Encomenda e : enc){
            this.encomendas.add(e.clone());
        }
    }
    
    /**
     * Clone
     */
    public Comprador clone(){
        return new Comprador(this);
    }
    
    /**
     * Equals
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Comprador c = (Comprador) o;
        return (super.equals(c) && this.encomendas.equals(c.getEncomenda()));
    }
    
    /**
     * toString
     */
    public String toString(){
        return "Comprador / " + super.toString();
    }
    
    /**
     * toFile
     */
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador:"+this.getEmail());
        sb.append(","+this.getPass());
        sb.append(","+this.getName());
        sb.append(","+this.getCoord().getLat());
        sb.append(","+this.getCoord().getLon());
        return(sb.toString());
    }
}