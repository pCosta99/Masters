package Model;

import java.io.Serializable;

/**
 * Representacao de Linha de Encomenda
 *
 * @author MaterialPOO
 * @version 20180312
 * @version 20200317
 */
public class LinhaEncomenda extends Produto implements Serializable {

    private double quantidade;

    /* Class constructors*/
    public LinhaEncomenda() {
        super();
        this.quantidade = 0;
    }
    public LinhaEncomenda(String referencia, String descricao, double preco, double quantidade) {
        super(referencia,descricao,preco);
        this.quantidade = quantidade;

    }
    public LinhaEncomenda(Produto produto, double quantidade) {
        super(produto);
        this.quantidade = quantidade;

    }
    public LinhaEncomenda(LinhaEncomenda linha) {
        super(linha.getReferencia(),linha.getDescricao(),linha.getPreco());
        this.quantidade = linha.getQuantidade();
    }

    /* Getters and Setters*/
    public double getQuantidade() {
        return this.quantidade;
    }
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /* Calcula o preço da linha de encomenda*/
    public double calculaValorLinhaEnc() {
        double valor = this.quantidade * this.getPreco();
        return valor;
    }

    @Override
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    @Override
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getReferencia().equals(this.getReferencia()) &&
                le.getDescricao().equals(this.getDescricao()) &&
                le.getPreco() == this.getPreco() &&
                le.getQuantidade() == this.quantidade;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Referencia: ").append(getReferencia());
        sb.append("Descricao: ").append(getDescricao());
        sb.append("Preco: ").append(getQuantidade());
        sb.append("Quantidade: ").append(this.quantidade);
        sb.append("\n");

        return sb.toString();
    }
}