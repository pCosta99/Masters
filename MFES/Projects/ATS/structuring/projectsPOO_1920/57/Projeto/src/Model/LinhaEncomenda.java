/**
 * classe que representa uma linhaEncomenda
 */
package Model;

import java.io.Serializable;
import java.util.Objects;

public class LinhaEncomenda implements Serializable {

    private String productCode;
    private String description;
    private double quantity;
    private double unitPrice;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public LinhaEncomenda() {
        this("", "", 0, 0);
    }

    public LinhaEncomenda(String productCode, String description, double quantity, double unitPrice) {
        this.productCode = productCode;
        this.description = description;
        this.quantity = quantity;
        this.unitPrice = unitPrice;
    }

    public LinhaEncomenda(LinhaEncomenda l) {
        this.productCode = l.getProductCode();
        this.description = l.getDescription();
        this.quantity = l.getQuantity();
        this.unitPrice = l.getUnitPrice();
    }

    //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * devolve prodCode
     * @return  codigo produto
     */
    public String getProductCode() {
        return productCode;
    }

    /**
     * devolve descrição do produto
     * @return descrição
     */
    public String getDescription() {
        return description;
    }

    /**
     * devolve quantidade
     * @return  quantidade
     */
    public double getQuantity() {
        return quantity;
    }

    /**
     * devolve unidades
     * @return uni
     */
    public double getUnitPrice() {
        return unitPrice;
    }

    /**
     * devolve preço
     * @return preço
     */
    public double getPrice() {
        return quantity * unitPrice;
    }

    //--------------------------------------------------------------equals, toString e clone--------------------------------------------------------------------------\\

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaEncomenda that = (LinhaEncomenda) o;
        return Double.compare(that.quantity, quantity) == 0 &&
                Double.compare(that.unitPrice, unitPrice) == 0 &&
                Objects.equals(productCode, that.productCode) &&
                Objects.equals(description, that.description);
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("");
        sb.append(productCode).append(": ");
        sb.append(description).append(" ");
        sb.append(quantity).append(" ");
        sb.append(unitPrice).append("€");
        return sb.toString();
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
}
