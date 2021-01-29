import java.io.Serializable;

/**
 * Classe que contém a informação sobre a linha de encomenda.
 */
public class LinhaEncomenda implements Serializable {

    private String codProduto;
    private String descricao;
    private double quantidade;
    private double precoU;

    public LinhaEncomenda() {
        this.codProduto = new String();
        this.descricao = new String();
        this.quantidade = 0.0;
        this.precoU = 0.0;
    }

    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double precoU) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.precoU = precoU;
    }

    public LinhaEncomenda(LinhaEncomenda o){
        this.codProduto = o.getCodProduto();
        this.descricao = o.getDescricao();
        this.precoU = o.getPrecoU();
        this.quantidade = o.getQuantidade();
    }

    public String getCodProduto() {
        return codProduto;
    }

    public String getDescricao() {
        return descricao;
    }

    public double getQuantidade() {
        return quantidade;
    }

    public double getPrecoU() {
        return precoU;
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

    public void setPrecoU(double precoU) {
        this.precoU = precoU;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaEncomenda that = (LinhaEncomenda) o;
        return Double.compare(that.getQuantidade(), quantidade) == 0 &&
                Double.compare(that.getPrecoU(), precoU) == 0 &&
                this.codProduto.equals(that.getCodProduto()) &&
                this.descricao.equals(that.getDescricao());
    }


    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append(this.codProduto).append(",");
        sb.append(this.descricao).append(",");
        sb.append(this.quantidade).append(",");
        sb.append(this.precoU).append(",");
        return sb.toString();
    }

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
}
