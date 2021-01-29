package Model.Catalogos;

import java.io.Serializable;
import java.util.Objects;

public class Produto implements IProduto, Serializable {
    private String codProduto;
    private String nome;
    private float preco;

    public Produto(){
        this.codProduto = " ";
        this.nome = " ";
        this.preco = 0;
    }

    public Produto(String codProduto, String nome, float preco){
        this.codProduto = codProduto;
        this.nome = nome;
        this.preco = preco;
    }

    public Produto(IProduto prod){
        this.codProduto = prod.getCodProduto();
        this.nome = prod.getNome();
        this.preco = prod.getPreco();
    }

    public String getCodProduto() {
        return this.codProduto;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public String getNome() {
        return this.nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public float getPreco() {
        return this.preco;
    }

    public void setPreco(float preco) {
        this.preco = preco;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Produto)) return false;
        Produto produto = (Produto) o;
        return getPreco() == produto.getPreco() &&
                Objects.equals(getCodProduto(), produto.getCodProduto()) &&
                Objects.equals(getNome(), produto.getNome());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getCodProduto(), getNome(), getPreco());
    }

    public String toString() {
        return "Produto:" +
                codProduto ; /*+ "\n" +
                nome + "\n" +
                preco;*/
    }

    public void criaProduto(String codProduto ,String nome, float preco){
        this.codProduto=codProduto;
        this.nome=nome;
        this.preco=preco;
    }


}
