/**
 * Write a description of class Proposta here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 3 - 09-06-2020
 */

import java.io.Serializable;

public class Proposta implements Serializable
{
    private String codigoProposta;
    private EmpresaTransportadora empresa;
    private double preco;
    private double tempo;
    private double distancia;
    private boolean aceite;
    private Encomenda encomenda;
    
    public Proposta ()
    {   this.codigoProposta = new String();
        this.empresa = new EmpresaTransportadora();
        this.preco = 0.0;
        this.tempo = 0.0;
        this.distancia = 0.0;
        this.aceite = false;
        this.encomenda = new Encomenda();
    }
    
    public Proposta (String codigoProposta, EmpresaTransportadora empresa, double preco, double tempo, double distancia, boolean aceite, Encomenda encomenda)
    {   this.codigoProposta = codigoProposta;
        this.empresa = empresa;
        this.preco = preco;
        this.tempo = tempo;
        this.distancia = distancia;
        this.aceite = aceite;
        this.encomenda = encomenda;
    }
    
    public Proposta (Proposta p)
    {   this.codigoProposta = p.getCodigoProposta();
        this.empresa = p.getEmpresa();
        this.preco = p.getPreco();
        this.tempo = p.getTempo();
        this.distancia = p.getDistancia();
        this.aceite = p.getAceite();
        this.encomenda = p.getEncomenda();
    }
    
    public String getCodigoProposta ()
    {   return this.codigoProposta;
    }
    
    public EmpresaTransportadora getEmpresa ()
    {   return this.empresa;
    }
    
    public double getPreco ()
    {   return this.preco;
    }
    
    public double getTempo ()
    {   return this.tempo;
    }
    
    public double getDistancia ()
    {   return this.distancia;
    }
    
    public boolean getAceite ()
    {   return this.aceite;
    }
    
    public Encomenda getEncomenda ()
    {   return this.encomenda;
    }
    
    public void setCodigoProposta (String codigoProposta)
    {   this.codigoProposta = codigoProposta;
    }
    
    public void setEmpresa (EmpresaTransportadora empresa)
    {   this.empresa = empresa;
    }
    
    public void setPreco (double preco)
    {   this.preco = preco;
    }
    
    public void setDistancia (double distancia)
    {   this.preco = distancia;
    }
    
    public void setAceite (boolean aceite)
    {   this.aceite = aceite;
    }
    
    public void setEncomenda (Encomenda encomenda)
    {   this.encomenda = encomenda;
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Proposta :").append(this.codigoProposta).append("\n").append("Empresa Transportadora: ").append(this.empresa).append("\n")
                                           .append("Preco: ").append(this.preco).append("\n")
                                           .append("Tempo: ").append(this.tempo).append("\n")
                                           .append("Distancia: ").append(this.distancia).append("\n");
        return s.toString();
    }
    
    public Proposta clone ()
    {   return new Proposta(this);
    }
}