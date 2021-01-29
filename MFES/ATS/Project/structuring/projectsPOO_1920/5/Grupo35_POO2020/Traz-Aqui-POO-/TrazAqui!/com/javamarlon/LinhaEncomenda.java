package com.javamarlon;

public class LinhaEncomenda {

    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;

    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double valorUnitario) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }

    @Override
    public String toString() {
        return "Código do produto: " + codProduto +
                ", Descrição: " + descricao +
                ", Quantidade: " + quantidade +
                ", Valor unitário: " + valorUnitario;
    }
}
