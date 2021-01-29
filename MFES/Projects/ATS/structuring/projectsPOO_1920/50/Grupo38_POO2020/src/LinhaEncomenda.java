import java.io.Serializable;

public class LinhaEncomenda  implements Serializable {
    private String cproduto;
    private String desc;
    private double qnt;
    private double preco;

    public String getCproduto() {
        return cproduto;
    }

    public void setCproduto(String cproduto) {
        this.cproduto = cproduto;
    }

    public String getDesc() {
        return desc;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public double getQnt() {
        return qnt;
    }

    public void setQnt(double qnt) {
        this.qnt = qnt;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public LinhaEncomenda(String cproduto, String desc, double qnt, double preco) {
        this.cproduto = cproduto;
        this.desc = desc;
        this.qnt = qnt;
        this.preco = preco;
    }

    public LinhaEncomenda(LinhaEncomenda a) {
        this.cproduto = a.getCproduto();
        this.desc = a.getDesc();
        this.qnt = a.getQnt();
        this.preco = a.getPreco();
    }

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Linha Encomenda: ")
                .append(this.cproduto).append(", ")
                .append(this.desc).append(", ")
                .append(this.preco).append(", ")
                .append(this.qnt);
        return sb.toString();
    }
}