import java.io.Serializable;

public class LinhaEncomenda implements Serializable
{
    private String codProduto;
    private String descricao;
    private double quantidade; // representa o peso
    private double precoUnitario; // preço p/kg
    
    /**
     * Construtor por omissao de objetos da classe LinhaEncomenda
     */
    public LinhaEncomenda()
    {
       this.codProduto = "";
       this.descricao = "";
       this.quantidade = 0; 
       this.precoUnitario = 0; 
    }
    
    /**
     * Construtor parametrizado de objetos da classe LinhaEncomenda
     */
    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double precoUnitario)
    {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.precoUnitario = precoUnitario;
    }
    
    /**
     * construtor de copia de objetos da classe LinhaEncomenda
     */
    public LinhaEncomenda(LinhaEncomenda linha)
    {
        this.codProduto = linha.getCodProd();
        this.descricao = linha.getDescricao();
        this.quantidade = linha.getQuantidade();
        this.precoUnitario = linha.getPrecoUnitario();
    }
   
    public String getCodProd()
    {
        return this.codProduto;
    }
    
    public String getDescricao()
    {
        return this.descricao;
    }
    
    public double getQuantidade()
    {
        return this.quantidade;
    }
    
    public double getPrecoUnitario()
    {
        return this.precoUnitario;
    }
    
    public void setCodProd(String novo_cod)
    {
        this.codProduto = novo_cod;
    }
    
    public void setDescricao(String nova_descricao)
    {
        this.descricao = nova_descricao;
    }
    
    public void setQuantidade(double nova_quantidade)
    {
        this.quantidade = nova_quantidade;
    }
    
    public void setPrecoUnitario(double novo_preco)
    {
        this.precoUnitario = novo_preco;
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("CodProduto: ").append(this.codProduto).append("\n");
        sb.append("Descricao: ").append(this.descricao).append("\n");
        sb.append("Quantidade: ").append(this.quantidade).append("\n");
        sb.append("Preco unitario: ").append(this.precoUnitario).append("\n");
        
        return sb.toString();
    }
    
    public LinhaEncomenda clone()
    {
        return new LinhaEncomenda(this);
    }
    
    public boolean equals(Object o)
    {
        if (o==this) return true;
        if (o==null || (o.getClass().equals(this.getClass())) == false) return false;
        LinhaEncomenda linha = (LinhaEncomenda)o;
        
        return linha.getCodProd().equals(this.codProduto) && linha.getDescricao().equals(this.descricao) &&
               linha.getQuantidade() == this.quantidade && linha.getPrecoUnitario() == this.precoUnitario;
    }


    public boolean eMedicamento (){
        if (this.descricao.equals("Benuron") || this.descricao.equals("Migretil")) return true;
        return false;
    }
}
