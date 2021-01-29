package model;

import exceptions.ExcecaoRegistoDeEncomendasNull;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class EmpresaTransporte implements Serializable {

    private String nome;
    private String id;
    private List<Encomenda> registoEncomendas;
    private double taxaKm; //taxa efetiva de uma empresa por km
    private double facturacao; // Facturacao total de uma empresa
    private int capacidadeEncomendas; //Capacidade de encomendas que uma empresa tem para transportar
    private Coordenadas localizacao;
    private double classificacao;

    public EmpresaTransporte() {
        this.nome = "";
        this.registoEncomendas = new ArrayList<Encomenda>();
        this.taxaKm = 0;
        this.facturacao = 0;
        this.capacidadeEncomendas = 0;
        this.localizacao = new Coordenadas();
        this.classificacao = 0.0;

    }


    public EmpresaTransporte(String nome,String id,List<Encomenda> reg, double taxaKm, int capacidadeEncomendas, Coordenadas localizacao, double classificacao) {
        this.nome = nome;
        this.id = id;
        setRegistoEncomendas(reg);
        this.taxaKm = taxaKm;
        this.capacidadeEncomendas = capacidadeEncomendas;
        this.localizacao = localizacao;
        this.facturacao = this.facturacaoTotal(); //já faz o calculo da facturaçao
        this.classificacao = classificacao;

    }

    public EmpresaTransporte(EmpresaTransporte k) {
        setNome(k.getNome());
        setId(k.getId());
        setCapacidadeEncomendas(k.getCapacidadeEncomendas());
        setLocalizacao(k.getLocalizacao());
        setTaxaKm(k.getTaxaKm());
        setFacturacao(k.facturacaoTotal());
        setClassificacao(k.getClassificacao());
        setCapacidadeEncomendas(k.getCapacidadeEncomendas());
    }


    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setRegistoEncomendas(List<Encomenda> reg) {
        this.registoEncomendas = new ArrayList<>();
        for (Encomenda p : reg) this.registoEncomendas.add(p);
    }


    public List<Encomenda> getRegistoEncomendas() {
        List<Encomenda> encomendas = new ArrayList<>();
        for(Encomenda enc: registoEncomendas){
            encomendas.add(enc.clone());
        }
        return encomendas;

    }

    public void addEncomendaEmpresa(Encomenda e){
        this.registoEncomendas.add(e);
    }

    public int getCapacidadeEncomendas() {
        return capacidadeEncomendas;
    }

    public void setCapacidadeEncomendas(int capacidadeEncomendas) {
        this.capacidadeEncomendas = capacidadeEncomendas;
    }

    public double getClassificacao() {
        return classificacao;
    }

    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    public double getFacturacao() {
        return facturacao;
    }

    public void setFacturacao(double facturacao) {
        this.facturacao = facturacao;
    }

    public Coordenadas getLocalizacao() {
        return localizacao;
    }

    public void setLocalizacao(Coordenadas localizacao) {
        this.localizacao = localizacao;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getTaxaKm() {
        return taxaKm;
    }

    public void setTaxaKm(double taxaKm) {
        this.taxaKm = taxaKm;
    }

    public boolean equals(Object obj) {
        if (obj == this) return true;

        if (obj == null || obj.getClass() != this.getClass()) return false;

        EmpresaTransporte empresa = (EmpresaTransporte) obj;

        return  this.getCapacidadeEncomendas() == empresa.getCapacidadeEncomendas() &&
                this.getId().equals(empresa.getId()) &&
                this.getClassificacao() == empresa.getClassificacao() &&
                this.getFacturacao() == empresa.getFacturacao() &&
                this.getNome().equals(empresa.getNome()) &&
                this.getLocalizacao().equals(empresa.getLocalizacao()) &&
                this.getTaxaKm() == empresa.getTaxaKm() &&
                this.getRegistoEncomendas().equals(empresa.getRegistoEncomendas());

    }


    public String toString(){
        StringBuilder sb = new StringBuilder();


        sb.append(getNome()).append("\n");
        sb.append(this.getId()).append("\n");
        sb.append(this.getFacturacao()).append("\n");
        sb.append(this.getTaxaKm()).append("\n");
        sb.append(this.getCapacidadeEncomendas()).append("\n");
        sb.append(this.getLocalizacao()).append("\n");
        sb.append(this.getClassificacao()).append("\n");
        return sb.toString();
    }

    /*
    public String topToString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getId()).append("\n");
        sb.append(this.numeroEncomendas()).append("\n");
        return sb.toString();
    }

     */

    public List<Encomenda> registoEncomendaemPeriodo(LocalDateTime inicio, LocalDateTime fim){
        return this.getRegistoEncomendas().stream().filter(encomenda -> encomenda.getData().isEqual(inicio) && encomenda.getData().isAfter(inicio)
                && encomenda.getData().isEqual(fim) && encomenda.getData().isBefore(fim)).collect(Collectors.toList());
    }



    public EmpresaTransporte clone(){
        return new EmpresaTransporte(this);
    }

    public double facturacaoTotal() {
        int total = 0;

        for (Encomenda e : this.getRegistoEncomendas()) {
                total += e.custoReal();
            }
            return total;
    }


    public int numeroEncomendas(){ return this.registoEncomendas.size();}



}





