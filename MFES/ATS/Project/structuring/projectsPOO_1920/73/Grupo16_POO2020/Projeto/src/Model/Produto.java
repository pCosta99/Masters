package Model;

import java.io.Serializable;

public class Produto implements Comparable<Produto>, Serializable {
    private String codProduto;
    private String descricao;

    public Produto(String codProduto, String descricao) {
        this.codProduto = codProduto;
        this.descricao = descricao;
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


    public int compareTo(Produto prod) {
        return Integer.compare(Integer.parseInt(this.codProduto.substring(1)),Integer.parseInt(prod.getCodProduto().substring(1)));
    }

}
