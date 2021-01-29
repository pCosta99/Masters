/**
 * Write a description of class SistemaDeEncomendas here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 3 - 09-06-2020
 */

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class LinhaDeEncomenda implements Serializable
{
    private String codigoProd;
    private String descricao;
    private double quantidade;
    private double valorUnitario;
    private double peso;
    private boolean encomendaMedica;
    
    public LinhaDeEncomenda ()
    {   this.codigoProd = new String();
        this.descricao = new String();
        this.quantidade = 0.0;
        this.valorUnitario = 0.0;
        this.peso = 0.0;
        this.encomendaMedica = false;
    }
    
    public LinhaDeEncomenda (String codigoProd, String descricao, double quantidade, double valorUnitario, double peso, boolean encomendaMedica)
    {   this.codigoProd = codigoProd;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
        this.peso = peso;
        this.encomendaMedica = encomendaMedica;
    }
    
    public LinhaDeEncomenda (LinhaDeEncomenda l)
    {   this.codigoProd = l.getCodigoProd();
        this.descricao = l.getDescricao();
        this.quantidade = l.getQuantidade();
        this.valorUnitario = l.getValorUnitario();
        this.peso = l.getPeso();
        this.encomendaMedica = l.getEncomendaMedica();
    }
    
    public String getCodigoProd ()
    {   return this.codigoProd;
    }
    
    public String getDescricao ()
    {   return this.descricao;
    }
    
    public double getQuantidade ()
    {   return this.quantidade;
    }
    
    public double getValorUnitario ()
    {   return this.valorUnitario;
    }
    
    public double getPeso ()
    {   return this.peso;
    }
    
    public boolean getEncomendaMedica ()
    {   return this.encomendaMedica;
    }
    
    public void setCodigoProd (String codigoProd)
    {   this.codigoProd = codigoProd;
    }
    
    public void setDescricao (String descricao)
    {   this.descricao = descricao;
    }
    
    public void setQuantidade (double quantidade)
    {   this.quantidade = quantidade;
    }
    
    public void setValorUnitario (double valorUnitario)
    {   this.valorUnitario = valorUnitario;
    }
    
    public void setPeso (double peso)
    {   this.peso = peso;
    }
    
    public void setEncomendaMedica (boolean encomendaMedica)
    {   this.encomendaMedica = encomendaMedica;
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Encomenda: ").append("\n").append("CodigoProduto: ").append(this.codigoProd)
                               .append(" , ").append("Descricao: ").append(this.descricao)
                               .append("\n").append("Quantidade: ").append(this.quantidade)
                               .append(" , ").append("Valor Unitario: ").append(this.valorUnitario)
                               .append(" , ").append("Peso: ").append(this.peso)
                               .append(" , ").append("Encomenda Medica: ").append(this.encomendaMedica).append("\n");
        return s.toString();
    }
    
    public boolean equals (Object o)
    {   if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LinhaDeEncomenda l = (LinhaDeEncomenda) o;
        return this.codigoProd.equals(l.getCodigoProd());
    }
    
    public LinhaDeEncomenda clone ()
    {   return new LinhaDeEncomenda(this);
    }
    
    public double getPrecoEncomenda()
    {   double precoEncomenda = 0.0;
        precoEncomenda = this.quantidade * this.valorUnitario;
        return precoEncomenda;
    }
}