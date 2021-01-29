import java.io.Serializable;
import java.util.ArrayList;
/**
 * Write a description of class encomenda here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

public class Encomenda extends Dados implements Serializable
{
    private String codEncomenda;
    private String codUser;
    private String codLoja;
    private double peso;
    private ArrayList<Produto> produtos;
    private ArrayList<Entregas> entregas;
    private ArrayList<Loja> lojas;
    private int estado;
    private String tipo;
    
    public Encomenda(){
        this.codEncomenda = new String();
        this.codUser = new String();
        this.codLoja =  new String();
        this.peso = 0;
        this.produtos = new ArrayList<Produto>();
        this.entregas = new ArrayList<Entregas>();
        this.lojas = new ArrayList<Loja>();
        this.estado = 0;
        this.tipo = new String();
    }
    
    public Encomenda(String cod, String cod1, String cod2,double p, ArrayList<Produto>Produtos,ArrayList<Entregas>entregas, ArrayList<Loja>loj,int est, String tipo){
        this.codEncomenda = cod;
        this.codUser = cod1;
        this.codLoja = cod2;
        this.peso = p;
        this.produtos = produtos;
        this.entregas = entregas;
        this.lojas = lojas;
        this.estado = est;
        this.tipo = tipo;
    }
    
    public Encomenda(Encomenda e){
        this.codEncomenda = e.getCodEncomenda();
        this.codUser = e.getCodUser();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.produtos = e.getProdutos();
        this.entregas = e.getEntregas();
        this.lojas = e.getAllLojas();
        this.estado = e.getEstado();
        this.tipo = e.getTipo();
    }

    public ArrayList<Produto> getProdutos(){
        ArrayList<Produto> l = new ArrayList<>();
        
        for(Produto li : produtos){
            l.add(li.clone());
        }
        return l;
    }
    public ArrayList<Entregas> getEntregas(){
        ArrayList<Entregas> en = new ArrayList<>();
        
        for(Entregas ent : entregas){
            en.add(ent.clone());
        }
        return en;
    }
    public ArrayList<Loja> getAllLojas(){
        ArrayList<Loja> l = new ArrayList<>();

        for(Loja li : lojas){
            l.add(li.clone());
        }
        return l;
    }
    public String getCodEncomenda(){ return this.codEncomenda;}
    public String getCodUser(){return this.codUser;}
    public String getCodLoja(){return this.codLoja;}
    public double getPeso(){return this.peso;}
    public int getEstado(){return this.estado;}
    public String getTipo(){return this.tipo;}
    
    public Encomenda clone(){ return new Encomenda(this);}

    public void setCodEncomenda(String c ){this.codEncomenda = c;}
    public void setCodUser(String c){this.codUser=c;}
    public void setCodLoja(String c){this.codLoja = c;}
    public void setPeso(double p){this.peso = p;}
    public void setEstado(int e){this.estado = e;}
    public void setTipo(String t){this.tipo = t;}
    public void setProdutos(ArrayList<Produto> produtos){
        this.produtos = new ArrayList<Produto>(produtos.size());
        for(Produto p : produtos) this.produtos.add(p.clone());
    }
    public void setEntregas(ArrayList<Entregas> entregas){
        this.entregas = new ArrayList<Entregas>(entregas.size());
        for(Entregas e : entregas) this.entregas.add(e.clone());
    }
    public void setAllLojas(ArrayList<Loja> lojas){
        this.lojas = new ArrayList<Loja>(lojas.size());
        for(Loja l : lojas) this.lojas.add(l.clone());
    }
    
    /**
     * Metodo que verifica se duas encomendas sao iguais
     * 
     * @param  o   o objeto a comparar
     * 
     * @return     o resultado da comparacao dos duas encomendas
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        else{
            Encomenda a = (Encomenda) o;
            return(a.getCodEncomenda().equals(this.getCodEncomenda())
                    && a.getCodUser().equals(this.getCodUser())
                    && a.getCodLoja().equals(this.getCodLoja())
                    && a.getPeso()== this.getPeso()
                    && a.getProdutos() == this.getProdutos()
                    && a.getEntregas() == this.getEntregas()
                    && a.getAllLojas() == this.getAllLojas()
                    && a.getEstado()== this.getEstado()
                    && a.getTipo()== this.getTipo());
        }
    }
    
    /**
     * Metodo que converte uma encomenda para uma string
     * 
     * @return    a encomenda em string
     */
    public String toString(){
        String aux = "Codigo da encomenda: " + this.codEncomenda + ";\n"
                     + "Codigo do Utilizador que solicitou a encomenda: " + this.codUser + ";\n"
                     + "Codigo da Loja associada ao pedido de encomenda:" + this.codLoja + ";\n"
                     + "Peso da Encomenda: " + this.peso + ";\n"
                     + "Estado: " +  this.estado + ";\n"
                     + "Tipo de encomenda: " + this.tipo + ";\n";
        return aux;
    }
    
    /**
     * Metodo que devolve o codigo de hash para uma encomenda
     * 
     * @return     o hashcode
     */
    public int hashCode(){
        int hash = 7; 
        hash = 31 * hash + codEncomenda.hashCode();
        hash = 31 * hash + codUser.hashCode();
        hash = 31 * hash + codLoja.hashCode();
        long aux = Double.doubleToLongBits(peso);
        hash = 31 * hash + (int)(aux ^ (aux >>> 32));
        long aux1 = Double.doubleToLongBits(estado);
        hash = 31 * hash + tipo.hashCode();
        return hash;
    }
    
    /**
     * Metodo que implementa a ordem natural de comparacao de instancias de AluguerCarro
     */
    public int compareTo(Encomenda e){
        return e.getCodEncomenda().compareTo(this.codEncomenda);
    }
    
    
}
