/**
 * classe que representa um produto
 */
package Model;

import java.io.Serializable;
import java.util.Objects;

public class Produto implements Serializable {
    private String prodCode;
    private String name;
    private double weight;
    private double price;
    private boolean isMedic;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public Produto() {
        this.prodCode = "";
        this.name = "";
        this.weight = 0d;
    }

    public Produto(String prodCode, String name, double weight,double price,boolean isMedic) {
        this.prodCode = prodCode;
        this.name = name;
        this.weight = weight;
        this.price = price;
        this.isMedic = isMedic;
    }

    public Produto(Produto p) {
        this.prodCode = p.getProdCode();
        this.name = p.getName();
        this.weight = p.getWeight();
        this.price = p.getPrice();
        this.isMedic = p.isMedic();
    }

    //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * devolve codigo de produto
     * @return codigo produto
     */
    public String getProdCode() {
        return prodCode;
    }

    /**
     * devolve nome
     * @return nome
     */
    public String getName() {
        return name;
    }

    /**
     * devolve peso
     * @return peso
     */
    public double getWeight() {
        return weight;
    }

    /**
     * devolve peso
     * @return peso
     */
    public double getPrice() {
        return price;
    }

    /**
     * devolve isMedic
     * @return isMedic
     */
    public boolean isMedic() {
        return isMedic;
    }

    //--------------------------------------------------------------toString, equals e clone--------------------------------------------------------------------------\\

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Produto{");
        sb.append("prodCode='").append(prodCode).append('\'');
        sb.append(", name='").append(name).append('\'');
        sb.append(", weight=").append(weight);
        sb.append('}');
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return Double.compare(produto.weight, weight) == 0 &&
                Objects.equals(prodCode, produto.prodCode) &&
                Objects.equals(name, produto.name);
    }

    public Produto clone() {
        return new Produto(this);
    }
}
