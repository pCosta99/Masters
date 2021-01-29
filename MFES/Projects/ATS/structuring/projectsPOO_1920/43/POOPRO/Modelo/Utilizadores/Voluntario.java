package Modelo.Utilizadores;

import Modelo.Encomendas.Encomenda;
import Modelo.Encomendas.RegistoEncomendas;

import java.util.List;

public class Voluntario extends Utilizador {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private double raioAcao;
    private double classificacao;
    private int nrClassificacoes;
    private boolean disponivel;

    /**
     * CONSTRUTOR VAZIO
     */

    public Voluntario() {
        super();
        this.raioAcao = 0.0;
        this.classificacao = 0.0;
        this.nrClassificacoes = 0;
        this.disponivel = false;
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 1
     */

    public Voluntario(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude) {
        super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude);
        this.raioAcao = 0.0;
        this.classificacao = 0;
        this.nrClassificacoes = 0;
        this.disponivel = false;
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 2
     */

    public Voluntario(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, RegistoEncomendas nRegisto, double nLatitude, double nLongitude, double nRaioAcao, int nClassificacao, int nNrClassificacoes, boolean nDisponivel, List<Encomenda> nPedidos) {
        super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude, nPedidos, nRegisto);
        this.raioAcao = nRaioAcao;
        this.classificacao = nClassificacao;
        this.nrClassificacoes = nNrClassificacoes;
        this.disponivel = nDisponivel;
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 3
     */

    public Voluntario(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude, double raioAcao) {
        super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude);
        this.raioAcao = raioAcao;
        this.classificacao = 0;
        this.nrClassificacoes = 0;
        this.disponivel = false;
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public Voluntario(Voluntario nVoluntario) {
        super(nVoluntario);
        this.raioAcao = nVoluntario.getRaioAcao();
        this.classificacao = nVoluntario.getClassificacao();
        this.nrClassificacoes = nVoluntario.getNrClassificacoes();
        this.disponivel = nVoluntario.isDisponivel();
    }

    /**
     * GETTERS
     */

    public double getRaioAcao() {
        return this.raioAcao;
    }

    public double getClassificacao() {
        return this.classificacao;
    }

    public int getNrClassificacoes() {
        return this.nrClassificacoes;
    }

    public boolean isDisponivel() {
        return this.disponivel;
    }

    /**
     * SETTERS
     */

    public void setRaioAcao(double raioAcao) {
        this.raioAcao = raioAcao;
    }

    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    public void setNrClassificacoes(int nrClassificacoes) {
        this.nrClassificacoes = nrClassificacoes;
    }

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    /**
     * MÉTODO CLONE
     */

    public Voluntario clone() {
        return new Voluntario(this);
    }

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Voluntario that = (Voluntario) o;
        return  super.equals(that) && Double.compare(that.raioAcao, raioAcao) == 0 &&
                classificacao == that.classificacao &&
                nrClassificacoes == that.nrClassificacoes &&
                disponivel == that.disponivel;
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("Voluntario{");
        sb.append(" RaioAcao=").append(this.raioAcao);
        sb.append(", Classificacao=").append(this.classificacao);
        sb.append(", nrClassificacao=").append(this.nrClassificacoes);
        sb.append(", Está disponivel=").append(this.disponivel);
        sb.append(", " + super.toString());
        sb.append('}');
        return sb.toString();
    }
}