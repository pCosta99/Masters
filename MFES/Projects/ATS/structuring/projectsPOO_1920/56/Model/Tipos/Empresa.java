package Model.Tipos;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

public class
Empresa extends Tipo implements IEmpresa, Serializable {
    private int nif;
    private double raio;
    private double preco; // preco por km

    private boolean disponibilidade;
    private boolean aceitaMedicamento;
    private double taxaAdicional;
    private double taxaChuva;
    private boolean chuva;
    private int nEncs;
    private double precoPeso;
    private int classificacao;


    public Empresa () {

        this.nif = 0;
        this.preco = 0.0;
        this.raio = 0.0;
        this.disponibilidade = true;

        this.taxaChuva = 1.5;
        this.aceitaMedicamento = false;
        this.taxaAdicional = 0.0;
        this.chuva = false;
        this.nEncs = 0;
        this.classificacao = 0;
        this.precoPeso = 0.5;
    }

    public Empresa (String id,int nif, boolean aceitaMedicamento, double preco, float longitude,
                    float latitude, double raio, double taxaAdicional, boolean chuva, int n, String nome) {

        super(id,nome,longitude,latitude);
        this.nif = nif;
        this.preco = preco;
        this.raio = raio;


        this.aceitaMedicamento = aceitaMedicamento;
        this.taxaAdicional = taxaAdicional;
        this.chuva = chuva;
        this.nEncs = n;
    }

    public Empresa (Empresa emp) {
        super(emp);

        this.aceitaMedicamento = emp.getAceitaMedicamento();
        this.raio = emp.getRaio();
        this.preco = emp.getPreco();
        this.chuva = emp.getChuva();
        this.taxaAdicional = emp.getTaxaAdicional();
        this.nif = emp.getNif();
        this.nEncs = emp.getNEncs();
    }

    //Getters e Setters


    public double getTaxaChuva() {
        return taxaChuva;
    }

    public void setTaxaChuva(double taxaChuva) {
        this.taxaChuva = taxaChuva;
    }

    public int getNif() {
        return this.nif;
    }
    public double getPreco() {
        return preco;
    }
    public double getRaio() {
        return this.raio;
    }

    public void setNif(int nif) {
        this.nif = nif;
    }
    public void setRaio(double raio) {
        this.raio = raio;
    }
    public void setPreco(double preco) {
        this.preco = preco;
    }

    //Variaveis para a App
    public boolean getAceitaMedicamento() {
        return this.aceitaMedicamento;
    }


    public boolean getChuva() {

        return this.chuva;
    }
    public double getTaxaAdicional() {
        return this.taxaAdicional;
    }


    public void setAceitaMedicamento(boolean aceitaMedicamento) {
        this.aceitaMedicamento = aceitaMedicamento;
    }

    public void setTaxaAdicional(double taxaAdicional) {
        this.taxaAdicional = taxaAdicional;
    }

    public void setChuva(boolean chuva) {
        this.chuva = chuva;
    }


    public boolean getDisponibilidade() {
        return disponibilidade;
    }

    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    public int getClassificacao() {
        return classificacao;
    }

    public void setClassificacao(int classificacao) {
        this.classificacao = classificacao;
    }

    public int getNEncs() {
        return nEncs;
    }

    public void setNEncs(int nEncs) {
        this.nEncs = nEncs;
    }

    public double getPrecoPeso() {
        return precoPeso;
    }

    public void setPrecoPeso(double precoPeso) {
        this.precoPeso = precoPeso;
    }

    public Empresa clone () {
        return new Empresa(this);
      }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Empresa)) return false;
        Empresa empresa = (Empresa) o;
        return  super.equals(empresa) &&
                getNif() == empresa.getNif() &&
                Double.compare(empresa.getRaio(), getRaio()) == 0 &&
                Double.compare(empresa.getPreco(), getPreco()) == 0 &&
                Objects.equals(getNome(), empresa.getNome());
    }


    //Transportadora:<CodEmpresa>,<NomeEmpresa>,<GPS>,<NIF>,<raio>,<preco-por-km>
    public String toString() {
        return "Transportadora:" +
                this.getId()+ "," +
                this.getNome() + "," +
                this.getX() + "," +
                this.getY() + "," +
                nif + "," +
                raio + "," +
                preco;
    }

}
