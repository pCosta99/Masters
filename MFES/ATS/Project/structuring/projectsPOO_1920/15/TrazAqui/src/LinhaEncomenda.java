import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;

    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double valorUnitario) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }

    public LinhaEncomenda(LinhaEncomenda e){
        this.codProduto = e.getCodProduto();
        this.descricao = e.getDescricao();
        this.quantidade = e.getQuantidade();
        this.valorUnitario = e.getValorUnitario();
    }

    public String getCodProduto() {
        return codProduto;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public String getDescricao() {
        return descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public double getQuantidade() {
        return quantidade;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public double getValorUnitario() {
        return valorUnitario;
    }

    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(" ");
        sb.append("CodProduto: ").append(this.codProduto).append(", ");
        sb.append("Descrição: ").append(this.descricao).append(", ");
        sb.append("Quantidade: ").append(this.quantidade).append(", ");
        sb.append("Preço: ").append(this.valorUnitario);
        return sb.toString();
    }


    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
}
