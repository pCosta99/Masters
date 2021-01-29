import java.util.*;
import java.io.*;

public class Viagem  implements Serializable {

    private Utilizador utilizador;
    private Loja loja;
    private Encomenda encomenda;
    private double dist;
    
    public Viagem(){
        this.utilizador= new Utilizador();
        this.loja = new Loja();
        this.encomenda = new Encomenda();
        this.dist = 0;
    }
    
    public Viagem(Utilizador utilizador, Loja loja,Encomenda encomenda,double dist){
        this.utilizador = utilizador;
        this.loja = loja;
        this.encomenda=encomenda;
        this.dist = dist;
    }
    
    public Viagem(Viagem v){
        this.utilizador = v.getUtilizador();
        this.loja = v.getLoja();
        this.encomenda=v.getEncomenda();
        this.dist = v.getDist();
    }
    
    public Utilizador getUtilizador(){return this.utilizador;}
    
    public Loja getLoja(){return this.loja;}
    
    public Encomenda getEncomenda(){return this.encomenda;}
    
    public double getDist(){return this.dist;}
    
    public void setUtilizador(Utilizador u){this.utilizador = u;}
    
    public void setLoja (Loja l){this.loja=l;}
    
    public void setEncomenda (Encomenda e){this.encomenda = e;}
    
    public void setDist (double d){this.dist=d;}

    public String toString() {
        return "Viagem{" +
                "utilizador=" + utilizador +
                ", loja=" + loja +
                ", dist=" + dist +
                '}';
    }

    public boolean equals (Object obj){
        if(this == obj) return true;
        if ((obj == null) || ( this.getClass()!= obj.getClass())) return false;
        Viagem v =(Viagem) obj;
        return (this.utilizador.equals(v.getUtilizador())&&
                 this.loja.equals(v.getLoja()) &&
                 this.dist == v.getDist());
    }
    
    
}
