package Modelo.Produtos;

import java.io.Serializable;
import java.nio.charset.Charset;
import java.util.Objects;
import java.util.Random;

public class Produto implements Serializable {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private String codProduto;
    private String descricao;
    private double valorUnitario;
    private double pesoUnitario;

    /**
     * CONSTRUTOR VAZIO
     */

    public Produto() {
        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.valorUnitario = 0.0;
        this.pesoUnitario = 0.0;
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 1
     */

    public Produto(String nDescricao, double nValorUnitario, double nPesoUnitario) {

        String AlphaNumericString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "0123456789" + "abcdefghijklmnopqrstuvxyz";

        StringBuilder sb = new StringBuilder(5);

        for (int i = 0; i < 5; i++) {
            int index = (int)(AlphaNumericString.length() * Math.random());
            sb.append(AlphaNumericString.charAt(index));
        }

        this.codProduto = sb.toString();
        this.descricao = nDescricao;
        this.valorUnitario = nValorUnitario;
        this.pesoUnitario = nPesoUnitario;
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 2
     */

    public Produto(String nCodProduto, String nDescricao, double nValorUnitario, double nPesoUnitario) {
        this.codProduto = nCodProduto;
        this.descricao = nDescricao;
        this.valorUnitario = nValorUnitario;
        this.pesoUnitario = nPesoUnitario;
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public Produto(Produto nProduto) {
        this.codProduto = nProduto.getCodProduto();
        this.descricao = nProduto.getDescricao();
        this.valorUnitario = nProduto.getValorUnitario();
        this.pesoUnitario = nProduto.getPesoUnitario();
    }

    /**
     * GETTERS
     */

    public String getCodProduto() {
        return this.codProduto;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public double getValorUnitario() {
        return this.valorUnitario;
    }

    public double getPesoUnitario() {
        return this.pesoUnitario;
    }

    /**
     * SETTERS
     */

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    public void setPesoUnitario(double pesoUnitario) {
        this.pesoUnitario = pesoUnitario;
    }

    /**
     * MÉTODO CLONE
     */

    public Produto clone() {
        return new Produto(this);
    }

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return Double.compare(produto.valorUnitario, valorUnitario) == 0 &&
                Double.compare(produto.pesoUnitario, pesoUnitario) == 0 &&
                Objects.equals(codProduto, produto.codProduto) &&
                Objects.equals(descricao, produto.descricao);
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        return "Produto{" +
                "codProduto='" + codProduto + '\'' +
                ", descricao='" + descricao + '\'' +
                ", valorUnitario=" + valorUnitario +
                ", pesoUnitario=" + pesoUnitario +
                '}';
    }
}
