import java.io.Serializable;
/**
 * descrição da classe LinhaEncomenda.
 */
/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class LinhaEncomenda implements Serializable{
    /** codigo do produto */
    private String codProduto;
    /** descriçao do produto*/
    private String descricao;
    /** preço do produto*/
    private double valorUnitario;
    /** quantidade de um produto*/
    private double quantidade;
    
    
    /**
     * Construtor de veiculo sem parametros
     */
    public LinhaEncomenda(){
        codProduto = null;
        descricao = null;
        valorUnitario = 0.0;
        quantidade = 0.0;
    }
    
    /**
     * Construtor por parametro
     */
    public LinhaEncomenda(String codProduto, String descricao, double valorUnitario, 
    double quantidade){
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.valorUnitario = valorUnitario;
        this.quantidade = quantidade; 
    }
    
    /**
     * Construtor de copia 
     */
    public LinhaEncomenda(LinhaEncomenda le){
        codProduto = le.getCodProduto();
        descricao = le.getDescricao();
        valorUnitario = le.getValorUnitario();
        quantidade = le.getQuantidade();
    }
    
    
    /**
     * Get de codigo de produto
     */
    public String getCodProduto(){
        return codProduto;
    }
    
    /**
     * Get de descriçao 
     */
    public String getDescricao(){
        return descricao;
    }
    
    /**
     * Get do valor unitario 
     */
    public double getValorUnitario(){
        return valorUnitario;
    }
    
    /**
     * Get de quantidade 
     */
    public double getQuantidade(){
        return quantidade;
    }
    
    /**
     * Altera o codigo do produto
     */
    public void setCodProduto (String cp){
        codProduto = cp;
    }
    
    /**
     * Altera a descriçao
     */
    public void setDescricao(String d){
        descricao = d;
    }
    
    /**
     * Altera o valor unitario
     */
    public void setValorUnitario(double v){
        valorUnitario = v;
    }
    
    /**
     * Altera a quantidade 
     */
    public void setQuantidade(double q){
        quantidade = q;
    }
        
    
    /**
     * Implementaçao do metodo toString
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Código do produto: ").append(this.codProduto);
        sb.append(" Produto: "+descricao);
        sb.append(" Preço: "+valorUnitario);
        sb.append(" Quantidade: "+quantidade+"\n");
        return sb.toString();
    }   
    
    /**
     * Implementaçao do metodo equals 
     * Compara um objeto para ver se e uma linha de encomenda
     */
    public boolean equals(Object o){
        if(this == o) return true;
        if(this.getClass() != o.getClass()) return false;
        LinhaEncomenda a=(LinhaEncomenda)o;
        if(this.codProduto != a.getCodProduto()) return false;
        if(this.descricao != a.getDescricao()) return false;
        if(this.valorUnitario != a.getValorUnitario()) return false;
        if(this.quantidade != a.getQuantidade()) return false;
        return true;
    }
    
    /**
     * Metodo clone faz uma copia do objeto LinhaEncomenda
     */
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
    
    
    
    
    
    
    
    
        
    
}
