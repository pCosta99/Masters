package model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Encomenda implements Serializable {
    /* variáveis de instância que classificam uma encomenda */
    private String codEntidade_transportadora;
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<Linha_Encomenda> linhas;


    /* após entrega da encomenda */
    private LocalDateTime data_entrega;
    private boolean classificada;
    private double tempo; //tempo que demorou a entrega da encomenda
    private double preco; //preco associado a entrega
    private double dist_total;

    public Encomenda() {
        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso=0.0;
        this.linhas = new ArrayList<>();

        this.data_entrega = null;
        this.classificada = false;
        this.tempo = 0;
        this.preco = calculaPreçoTotal();
        this.dist_total = 0;
    }


    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, ArrayList<Linha_Encomenda> linhas) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso=peso;
        this.linhas=linhas;

        this.data_entrega = null;
        this.tempo = 0;
        this.preco = calculaPreçoTotal();
        this.dist_total = 0;
    }

    /**
     * Construtor parametrizado de uma encomenda.
     * Necessário para a criação de um Encomenda através da leitura do ficheiro de logs.
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, List<Linha_Encomenda> linhas) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso=peso;
        this.linhas=linhas;

        this.data_entrega = null;
        this.classificada = false;
        this.tempo = 0;
        this.preco = calculaPreçoTotal();
        this.dist_total = 0;
    }

    public Encomenda(Encomenda encomenda) {
        this.codEncomenda = encomenda.getCodEncomenda();
        this.codUtilizador = encomenda.getCodUtilizador();
        this.codLoja = encomenda.getCodLoja();
        this.peso= encomenda.getPeso();
        this.linhas=encomenda.getLinhas();
        this.data_entrega = encomenda.getData();
        this.classificada = encomenda.isClassificada();
        this.tempo = encomenda.getTempo();
        this.preco = encomenda.getPreco();
    }

    public String getCodEncomenda() {
        return codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getCodLoja() {
        return codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public LocalDateTime getData() {return this.data_entrega;}

    public void setData(LocalDateTime data) {this.data_entrega = data;}

    public boolean isClassificada() { return this.classificada; }

    public void setClassificada(boolean classificada) { this.classificada = classificada; }

    public ArrayList<Linha_Encomenda> getLinhas() {
        return this.linhas.stream().map(Linha_Encomenda::clone).collect(Collectors.toCollection(ArrayList::new));
    }

    public void setLinhas(List<Linha_Encomenda> l){
        this.linhas = l.stream().map(Linha_Encomenda::clone).collect(Collectors.toCollection(ArrayList::new));
    }

    public String getCodEntidade_transportadora() {
        return codEntidade_transportadora;
    }

    public void setCodEntidade_transportadora(String codEntidade_transportadora) {
        this.codEntidade_transportadora = codEntidade_transportadora;
    }

    public double getTempo() { return this.tempo; }

    public void setTempo(double tempo) { this.tempo = tempo; }

    public double getPreco() { return this.preco; }

    public void setPreco(double preco) { this.preco = preco; }

    public double getDist_total() { return this.dist_total; }

    public void setDist_total(double dist_total) { this.dist_total = dist_total; }

    public Encomenda clone(){
        return new Encomenda(this);
    }

    public double calculaPreçoTotal(){
        return this.linhas.stream().mapToDouble(Linha_Encomenda::calculaValorLinhaEnc).sum();
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("\nCod Encomenda: ").append(this.codEncomenda);
        sb.append("\nCódigo da Loja: ").append(this.codLoja);
        sb.append("\nPeso: ").append(this.peso);
        sb.append("\nLinhas: ");

        this.linhas.stream().forEach(linha -> sb.append(linha.toString()).append("\n"));

        return sb.toString();
    }



}
