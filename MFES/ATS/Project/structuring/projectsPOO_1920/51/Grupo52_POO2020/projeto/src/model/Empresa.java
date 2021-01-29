package model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


public class Empresa extends Entidade implements Serializable, LicencaMedica {
    private String codEmpresa;
    private double classificacao;
    private String nif;
    private double raio;
    private double precokm;
    private boolean disponivel;
    private boolean licenca;
    private double vkm = 65;
    private List<Encomenda> encomendas_por_sinalizar;
    private List<Encomenda> encomendas_entregues; //tempo de entrega + custo da entrega

    public Empresa(String email, String password, String nome, GPS gps, String nif, double raio, double precokm, int number) {
        super(email, password, nome, gps);
        this.codEmpresa = "l" + number;
        this.classificacao = 5;
        this.raio=raio;
        this.nif=nif;
        this.precokm=precokm;
        this.disponivel = true;
        this.licenca = false;
        this.encomendas_por_sinalizar = new ArrayList<>();
        this.encomendas_entregues = new ArrayList<>();
    }

    /**
     * Construtor parametrizado de um Voluntario.
     * Necessário para a criação de um Voluntario através da leitura do ficheiro de logs.
     */
    public Empresa(String codEmpresa, String nome, GPS gps, String nif, double raio, double precokm) {
        super(codEmpresa,nome, gps);
        this.codEmpresa=codEmpresa;
        this.nif=nif;
        this.raio=raio;
        this.precokm=precokm;
        this.disponivel = true;
        this.licenca = false;
        this.encomendas_por_sinalizar = new ArrayList<>();
        this.encomendas_entregues = new ArrayList<>();

    }

    public Empresa(Empresa empresa) {
        super(empresa.getEmail(), empresa.getPassword(), empresa.getNome(), empresa.getGps());
        this.codEmpresa = empresa.getCodEmpresa();
        this.classificacao = empresa.getClassificacao();
        this.nif=empresa.getNif();
        this.raio=empresa.getRaio();
        this.precokm=empresa.getPrecokm();
        this.disponivel = empresa.isDisponivel();
        this.licenca = empresa.isLicenca();
        this.encomendas_por_sinalizar = empresa.getEncomendas_por_sinalizar();
        this.encomendas_entregues = empresa.getEncomendas_entregues();

    }

    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public double getPrecokm() {
        return this.precokm;
    }

    public void setPrecokm(double precokm) {
        this.precokm = precokm;
    }

    public String getNif() {
        return this.nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public String getCodEmpresa() {
        return this.codEmpresa;
    }

    public void setCodEmpresa(String codloja) {
        this.codEmpresa = codloja;
    }

    public double getClassificacao() {
        return this.classificacao;
    }

    public void addClassificacao(double cl) {
        this.classificacao = (this.classificacao + cl) / 2;
    }

    public boolean isDisponivel() { return this.disponivel; }

    public void setDisponivel(boolean disponivel) { this.disponivel = disponivel; }

    public boolean isLicenca() {
        return licenca;
    }

    public void setLicenca(boolean licenca) {
        this.licenca = licenca;
    }


    public List<Encomenda> getEncomendas_por_sinalizar() {
        return this.encomendas_por_sinalizar.stream().collect(Collectors.toList());
    }

    public void addToEncomendasPorSinalizar (Encomenda e){
        this.encomendas_por_sinalizar.add(e);
    }

    public List<Encomenda> getEncomendas_entregues() {
        return this.encomendas_entregues.stream().map(Encomenda::clone).collect(Collectors.toList());
    }
    public double getVkm() { return this.vkm; }

    public void setVkm(double vkm) { this.vkm = vkm; }

    public void encomendaEntregue (Encomenda e){
        e.setData(LocalDateTime.now());
        e.setCodEntidade_transportadora(this.codEmpresa);
        this.encomendas_por_sinalizar.add(e);
    }


    @Override
    public boolean aceitoTransporteMedicamentos() { return this.licenca; }

    @Override
    public void aceitaMedicamentos(boolean state) { this.licenca = state; }


    public void sinalizarEncomenda(Encomenda encomenda){
        this.encomendas_por_sinalizar.remove(encomenda);
        this.encomendas_entregues.add(encomenda);
    }

    public List<Encomenda> allEncomendas(){
        List<Encomenda> res = new ArrayList<>();
        res.addAll(getEncomendas_entregues());
        res.addAll(getEncomendas_por_sinalizar());
        return res;
    }


    public double totalFaturado(LocalDateTime di, LocalDateTime df){
        double total = 0;

        total += this.encomendas_por_sinalizar.stream().filter(encomenda -> encomenda.getData().isAfter(di) && encomenda.getData().isBefore(df)).mapToDouble(Encomenda::calculaPreçoTotal).sum();
        total += this.encomendas_entregues.stream().filter(encomenda -> encomenda.getData().isAfter(di) && encomenda.getData().isBefore(df)).mapToDouble(Encomenda::calculaPreçoTotal).sum();

        return total;
    }


    public double totalKms(){
        double total = 0;

        total += this.encomendas_por_sinalizar.stream().mapToDouble(Encomenda::getDist_total).sum();
        total += this.encomendas_entregues.stream().mapToDouble(Encomenda::getDist_total).sum();

        return total;
    }


    public Empresa clone() { return new Empresa(this); }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nNome: ").append(this.getNome());
        sb.append("\nEmail: ").append(this.getEmail());
        sb.append("\nNIF: ").append(this.getNif());
        sb.append("\nLocalização: ").append(this.getGps());
        sb.append("\nRaio de ação: ").append(this.getRaio());
        sb.append("\nClassificação: ").append(this.getClassificacao());

        sb.append("\nLicença: ").append(this.isLicenca());
        sb.append("\nDisponiblidade: ").append(this.isDisponivel());

        return sb.toString();
    }

    public String toStringTOclients(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nNome: ").append(this.getNome());
        sb.append("\nClassificação: ").append(this.getClassificacao());
        sb.append("\nPreço: ").append(this.getPrecokm());
        return sb.toString();
    }
}
