import java.io.Serializable;

public class LinhaEncomenda implements Serializable {

    private String referencia;
    private String descricao;
    private double preco;
    private int quantidade;

    public LinhaEncomenda() {
        this.referencia = "n/a";
        this.descricao = "n/a";
        this.preco = 0;
        this.quantidade = 0;
    }

    public LinhaEncomenda(String referencia, String descricao, double preco,
                          int quantidade) {
        this.referencia = referencia;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
    }

    public LinhaEncomenda(LinhaEncomenda linha) {
        this.referencia = linha.getReferencia();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
    }

    public String getReferencia() {
        return this.referencia;
    }

    public void setReferencia(String referencia) {
        this.referencia = referencia;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public double getPreco() {
        return this.preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public int getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getReferencia().equals(this.referencia) &&
                le.getDescricao().equals(this.descricao) &&
                le.getQuantidade() == this.quantidade &&
                le.getPreco() == this.preco;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Referencia: ").append(this.referencia)
            .append("Descrição: ").append(this.descricao)
            .append("Quantidade: ").append(this.quantidade)
            .append("Preco: ").append(this.preco);
        return sb.toString();
    }
}
