/**
 * Write a description of class Encomenda here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 3 - 09-06-2020
 */

import java.lang.String;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class Encomenda implements Serializable
{
    private String codigoEnc;
    private String codigoUser;
    private String codigoLoja;
    private double peso;
    private ArrayList<LinhaDeEncomenda> produtos;
    private boolean encomendaMedica;
    private ArrayList<Proposta> propostas;
    
    public Encomenda ()
    {   this.codigoEnc = new String();
        this.codigoUser = new String();
        this.codigoLoja = new String();
        this.peso = 0.0;
        this.produtos = new ArrayList<LinhaDeEncomenda>();
        this.encomendaMedica = false;
        this.propostas = new ArrayList<Proposta>();
    }
    
    public Encomenda (String codigoEnc, String codigoUser, String codigoLoja, double peso, ArrayList<LinhaDeEncomenda> produtos, boolean encomendaMedica, ArrayList<Proposta> propostas)
    {   this.codigoEnc = codigoEnc;
        this.codigoUser = codigoUser;
        this.codigoLoja = codigoLoja;
        this.peso = peso;
        setProdutos(produtos);
        this.encomendaMedica = encomendaMedica;
        setPropostas(propostas);
    }
    
    public Encomenda (Encomenda e)
    {   this.codigoEnc = e.getCodigoEnc();
        this.codigoUser = e.getCodigoUser();
        this.codigoLoja = e.getCodigoLoja();
        this.peso = e.getPeso();
        setProdutos(e.getProdutos());
        this.encomendaMedica = e.getEncomendaMedica();
        setPropostas(e.getPropostas());
    }
    
    public String  getCodigoEnc ()
    {   return this.codigoEnc;
    }
    
    public String  getCodigoUser ()
    {   return this.codigoUser;
    }
    
    public String getCodigoLoja ()
    {   return this.codigoLoja;
    }
    
    public double getPeso ()
    {   return this.peso;
    }
    
    public List<LinhaDeEncomenda> getProdutos ()
    {   List<LinhaDeEncomenda> aux = new ArrayList<LinhaDeEncomenda>();
        for (LinhaDeEncomenda l : this.produtos){
            aux.add(l);
        }
        return aux;
    }
    
    public boolean getEncomendaMedica ()
    {   return this.encomendaMedica;
    }
    
    public List<Proposta> getPropostas ()
    {   List<Proposta> aux = new ArrayList<Proposta>();
        for (Proposta p : this.propostas){
            aux.add(p);
        }
        return aux;
    }
    
    public void setCodigoEnc (String codigoEnc)
    {   this.codigoEnc = codigoEnc;
    }
    
    public void setCodigoUser (String codigoUser)
    {   this.codigoUser = codigoUser;
    }
    
    public void setCodigoLoja (String codigoLoja)
    {   this.codigoLoja = codigoLoja;
    }
    
    public void setPeso (double peso)
    {   this.peso = peso;
    }
    
    public void setProdutos (List<LinhaDeEncomenda> produtos)
    {   this.produtos = new ArrayList<LinhaDeEncomenda>();
        for (LinhaDeEncomenda l : produtos){
            this.produtos.add(l);
        }
    }
    
    public void setPropostas (List<Proposta> propostas)
    {   this.propostas = new ArrayList<Proposta>();
        for (Proposta p : propostas){
            this.propostas.add(p);
        }
    }
    
    public void setEncomendaMedica (boolean encomendaMedica)
    {   this.encomendaMedica = encomendaMedica;
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Encomenda: ").append("\n").append(this.codigoEnc).append("\n")
                                            .append("Utilizador:").append(this.codigoUser).append("\n")
                                            .append("Vendedor: ").append(this.codigoLoja).append("\n")
                                            .append("Peso: ").append(this.peso).append("\n")
                                            .append("Produtos: ").append(this.produtos).append("\n")
                                            .append("Encomenda Medica: ").append(this.encomendaMedica).append("\n");
        return s.toString();
    }
    
    public boolean equals (Object o)
    {   boolean flag = false;
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda) o;
        if (this.peso == e.getPeso() && this.encomendaMedica == e.getEncomendaMedica()){
            flag = true;
        }
        return flag && this.codigoEnc.equals(e.getCodigoEnc()) && this.codigoUser.equals(e.getCodigoUser()) && this.codigoLoja.equals(e.getCodigoLoja()) 
                    && this.produtos.equals(e.getProdutos());
    }
    
    public Encomenda clone ()
    {   return new Encomenda(this);
    }
    
    public int compareTo (Encomenda e)
    {   int compareTo = 0;
        if (this.peso < e.getPeso()){
            compareTo = -1;
        }
        if (this.peso > e.getPeso()){
            compareTo = 1;
        }
        return compareTo;
    }
    
    public double getPrecoEncomenda ()
    {   double preco = 0.0;
        for (LinhaDeEncomenda l : this.produtos){
            preco = preco + l.getPrecoEncomenda();
        }
        return preco;
    }
}