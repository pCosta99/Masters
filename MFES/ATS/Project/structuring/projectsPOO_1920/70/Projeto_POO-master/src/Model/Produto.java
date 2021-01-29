package Model;

import java.util.Objects;

/** Declaration of Class Produto which contains the required information on each Product.
 *
 *  String referencia - The product's system code.
 *  String descricao - Brief description of the product.
 *  double preco - The price tag for this product.
 */
public class Produto {
    private String referencia;
    private String descricao;
    private double preco;

    /* Class Constructors*/
    public Produto() {
        this.referencia = "";
        this.descricao = "";
        this.preco = 0;
    }
    public Produto(String referencia, String descricao, double preco) {
        this.referencia = referencia;
        this.descricao = descricao;
        this.preco = preco;
    }
    public Produto(Produto p){
        this.referencia = p.getReferencia();
        this.descricao = p.getDescricao();
        this.preco = p.getPreco();
    }

    /* Getters and Setters*/
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return Double.compare(produto.preco, this.preco) == 0 &&
                this.referencia.equals(produto.referencia) &&
                this.descricao.equals(produto.descricao);
    }
    @Override
    public String toString() {
        return "Produto:" + referencia + "| Preco" + preco + "€" + "\n" + "Descricao= " + descricao + "\n" ;
    }
    @Override
    public Produto clone(){
        return new Produto(this);
    }
}
