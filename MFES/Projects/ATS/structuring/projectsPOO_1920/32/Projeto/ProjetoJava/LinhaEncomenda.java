import java.io.Serializable;
public class LinhaEncomenda implements Serializable
{
    private String codProduto;
    private String descricao;
    private double preco;
    private double quantidade;
    
    public LinhaEncomenda()
    {
      this.codProduto="n/a";
      this.descricao="n/a";
      this.preco=0;
      this.quantidade=0;   
    }
    public LinhaEncomenda(String codProd,String descricao,double quant,double preco)
    {
        this.codProduto=codProd;
        this.descricao=descricao;
        this.preco=preco;
        this.quantidade=quant;
    }
    public LinhaEncomenda (LinhaEncomenda umaEncomenda)
    {
        this.preco=umaEncomenda.getPreco();
        this.quantidade=umaEncomenda.getQuantidade();
        this.codProduto=umaEncomenda.getCodProduto();
        this.descricao=umaEncomenda.getDescricao();
    }
    public double getQuantidade ()
    {
      return this.quantidade;  
    }
    public double getPreco()
    {
     return this.preco;
    }
    public String getDescricao()
    {
     return this.descricao;
    }
    public String getCodProduto()
    {
    return this.codProduto;
    }
    public void setPreco (double novoPreco)
    {
    this.preco=novoPreco;
    }
    public void setQuantidade (double novoQuantidade)
    {
    this.quantidade=novoQuantidade;   
    }
    public void setCodProduto (String novoReferencia)
    {
    this.codProduto=novoReferencia;
    }
    public void setDescricao (String novoDescricao)
    {
       this.descricao=novoDescricao;
    }
    public String toString ()
    {
    StringBuilder sb =new StringBuilder();
    sb.append(",").append(this.codProduto);
    sb.append(",").append(this.descricao);
    sb.append(",").append(this.quantidade);
    sb.append(",").append(this.preco);
    return sb.toString();
    }
    public LinhaEncomenda clone()
    {
     return new LinhaEncomenda(this);   
    }
    public boolean equals (Object o)//deveria funcionar para qualquer objeto
    {
    if (this==o) return true;
    if ((o == null) || (this.getClass() != o.getClass()))
    return false;
    LinhaEncomenda p = (LinhaEncomenda) o;
    return (p.getPreco()==this.preco && 
    p.getCodProduto().equals(this.codProduto) &&
    p.getDescricao().equals(this.descricao) &&
    p.getQuantidade()==this.quantidade
     );
    }
    public double calculaValorLinhaEncomenda()
    {
    return this.getQuantidade()*this.getPreco();
    }   
}
