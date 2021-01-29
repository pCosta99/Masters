package Model.Encomendas;

import Model.Catalogos.IProduto;
import Model.Catalogos.Produto;

import java.io.Serializable;
import java.util.Objects;

public class LinhaEncomenda implements ILinhaEncomenda, Serializable {
    private IProduto produto;
    private float quantidade;
    private float valor;


    public LinhaEncomenda() {
        this.produto = new Produto();
        this.quantidade = 0;
        this.valor = 0;

    }

    public LinhaEncomenda(IProduto produto, Float quantidade, Float valor) {
        this.produto = produto;
        this.quantidade = quantidade;
        this.valor = valor;
    }

    public LinhaEncomenda(LinhaEncomenda linha){
        this.produto = linha.getProduto();
        this.quantidade = linha.getQuantidade();
        this.valor = linha.getValor();
    }

    public IProduto getProduto(){
        return this.produto;
    }
    public void setProduto(IProduto prod){
        this.produto = prod;
    }

    public float getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(float quantidade) {
        this.quantidade = quantidade;
    }

    public float getValor() {
        return this.valor;
    }

    public void setValor(float valor) {
        this.valor = valor;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaEncomenda that = (LinhaEncomenda) o;
        return Float.compare(that.quantidade, quantidade) == 0 &&
                Float.compare(that.valor, valor) == 0 &&
                Objects.equals(produto,that.produto);
    }

    public String toString() {
        return "Código produto:" + produto.getCodProduto() +
                "\nDescrição " +  produto.getNome() +
                "\nQuantidade: " + quantidade +
                "\nPreço: " + valor +"\n";
    }

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

    public void insereLinhaEncomenda (IProduto prod, String aux3, String aux4){
        this.produto = prod;
        this.quantidade = Float.parseFloat(aux3);
        this.valor = Float.parseFloat(aux4);
        //System.out.print(l.toString());
    }
}

