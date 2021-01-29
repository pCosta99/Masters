package Base.Encomenda;

public class LinhaEncomenda {
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;

    
    public LinhaEncomenda() {
        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.quantidade = 0;
        this.valorUnitario = 0;
    }
    
    public LinhaEncomenda(String codProduto, String descricao, int quantidade, double valorUnitario) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }
    
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.codProduto = linha.getCodProduto();
        this.descricao = linha.getDescricao();
        this.valorUnitario = linha.getValorUnitario();
        this.quantidade = linha.getQuantidade();
    }
    
    /**
     * B)
     */
    public double calculaValorLinhaEnc() {
        double valor = this.quantidade * this.valorUnitario;
        return valor;
    }
    
    public String getCodProduto() {
        return this.codProduto;
    }
    
    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public Double getValorUnitario() {
        return this.valorUnitario;
    }

    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    public Double getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(Double quantidade) {
        this.quantidade = quantidade;
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getCodProduto().equals(this.codProduto) &&
                le.getDescricao().equals(this.descricao) && 
                le.getValorUnitario() == this.valorUnitario;
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código de Produto: ").append(this.codProduto);
        //..
        return sb.toString();
    }            
    
}
