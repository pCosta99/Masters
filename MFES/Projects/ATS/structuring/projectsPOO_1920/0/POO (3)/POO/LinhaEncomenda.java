import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;

    public LinhaEncomenda() {
        this.codProduto = new String();
        this.descricao = new String();
        this.quantidade = 0;
        this.valorUnitario = 0;
    }

    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double valorUnitario) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }

    public LinhaEncomenda(LinhaEncomenda le) {
        this.codProduto = le.getCodProduto();
        this.descricao = le.getDescricao();
        this.quantidade = le.getQuantidade();
        this.valorUnitario = le.getValorUnitario();
    }

    public String getCodProduto() {
        return this.codProduto;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public double getValorUnitario() {
        return this.valorUnitario;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\tCodigo Produto: " + this.codProduto);
        s.append("\n\tDescricao: " + this.descricao );
        s.append("\n\tQuantidade : " + this.quantidade);
        s.append("\n\tValor Unitario:" + this.valorUnitario);
        return s.toString();
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if ((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return this.codProduto.equals(le.getCodProduto()) &&
                this.descricao.equals(le.getDescricao()) &&
                this.quantidade == le.getQuantidade() &&
                this.valorUnitario == le.getValorUnitario();
    }

}
