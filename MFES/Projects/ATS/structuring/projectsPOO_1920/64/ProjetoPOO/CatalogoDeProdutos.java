/**
 * Write a description of class Catalogo here.
 *
 * @author Francisco Luis Rodrigue- Claudino, A89493
 * @version Versao 1 - 09-06-2020
 */

import java.io.Serializable;

public class CatalogoDeProdutos implements Serializable
{
    private String nomeProduto;
    private String codigoProduto;
    private double peso;
    private boolean encomendaMedica;
    private double preco;
    
    public CatalogoDeProdutos ()
    {   this.nomeProduto = new String();
        this.codigoProduto = new String();
        this.peso = 0.0;
        this.encomendaMedica = false;
        this.preco = 0.0;
    }
    
    public CatalogoDeProdutos (String nomeProduto, String codigoProduto, double peso, boolean encomendaMedica ,double preco)
    {   this.nomeProduto = nomeProduto;
        this.codigoProduto = codigoProduto;
        this.peso = peso;
        this.encomendaMedica = encomendaMedica;
        this.preco = preco;
    }
    
    public CatalogoDeProdutos (CatalogoDeProdutos c)
    {   this.nomeProduto = c.getNomeProduto();
        this.codigoProduto = c.getCodigoProduto();
        this.peso = c.getPeso();
        this.encomendaMedica = c.getEncomendaMedica();
        this.preco = c.getPreco();
    }
    
    public String getNomeProduto ()
    {   return this.nomeProduto;
    }
    
    public String getCodigoProduto ()
    {   return this.codigoProduto;
    }
    
    public double getPeso ()
    {   return this.peso;
    }
    
    public boolean getEncomendaMedica ()
    {   return this.encomendaMedica;
    }
    
    public double getPreco ()
    {   return this.preco;
    }
    
    public void setNomeProduto (String nomeProduto)
    {   this.nomeProduto = nomeProduto;
    }
    
    public void setCodigoProduto (String codigoProduto)
    {   this.codigoProduto = codigoProduto;
    }
    
    public void setPeso (double peso)
    {   this.peso = peso;
    }
    
    public void setEncomendaMedica (boolean encomendaMedica)
    {   this.encomendaMedica = encomendaMedica;
    }
    
    public void setPreco (double preco)
    {   this.preco = preco;
    }
    
    public String toString ()
    {   StringBuilder s = new StringBuilder();
        s.append("Produto: ").append(this.nomeProduto).append(" , ").append("Preco: ").append(this.preco).append("\n");
        return s.toString();
    }
    
    public CatalogoDeProdutos clone ()
    {   return new CatalogoDeProdutos(this);
    }
}