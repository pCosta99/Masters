package com.javamarlon;

import java.util.ArrayList;

public class Encomenda {

    private String codEncomenda;
    private Utilizador utilizador;
    private Loja loja;
    private double peso;
    private ArrayList<LinhaEncomenda> linhaEncomendas;
    private int classificacao;

    public Encomenda(String codEncomenda, Utilizador utilizador, Loja loja, double peso, ArrayList<LinhaEncomenda> linhaEncomendas) {
        this.codEncomenda = codEncomenda;
        this.utilizador = utilizador;
        this.loja = loja;
        this.peso = peso;
        this.linhaEncomendas = linhaEncomendas;
        this.classificacao = -1;
    }

    public String getCodEncomenda() {
        return codEncomenda;
    }

    public Utilizador getUtilizador() {
        return utilizador;
    }

    public Loja getLoja() {
        return loja;
    }

    public double getPeso() {
        return peso;
    }

    public ArrayList<LinhaEncomenda> getLinhaEncomendas() {
        return linhaEncomendas;
    }

    public void setClassificacao(int classificacao) {
        this.classificacao = classificacao;
    }

    @Override
    public String toString() {
        return "\nCódigo da encomenda: " + codEncomenda +
                "\nCódigo do utilizador: " + utilizador.getCod() +
                "\nCódigo da Loja: " + loja.getCod() +
                "\nPeso da encomenda: " + peso +
                "\nConteúdo: " + linhaEncomendas;
    }
}
