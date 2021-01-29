package model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Utilizador extends Entidade implements Serializable {
    private String codUtilizador;
    private List<Encomenda> encomendas_standby; //stand by até haver um voluntário dentro do raio
    private List<Encomenda> encomendas_entregues; //entregue pronta para ser classificada

    /**
     * Construtor parametrizado de um Utilizador.
     * * Aceita como parâmetros cada componente necessária excepto o código de Utilizador.
     */
    public Utilizador(String email, String password, String nome, GPS gps, int number) {
        super(email, password, nome,gps);
        this.codUtilizador = "u" + number;
        this.encomendas_standby = new ArrayList<>();
        this.encomendas_entregues = new ArrayList<>();

    }

    /**
     * Construtor parametrizado de um Utilizador.
     * Necessário para a criação de um Utilizador através da leitura do ficheiro de logs.
     */
    public Utilizador(String codUtilizador, String nome, GPS gps) {
        super(codUtilizador,nome, gps);
        this.codUtilizador=codUtilizador;
        this.encomendas_standby = new ArrayList<>();
        this.encomendas_entregues = new ArrayList<>();
    }

    /**
     * Construtor de cópia, ou seja, copia os dados de um Utilizador já existente.
     * @param utilizador que vamos copiar.
     */
    public Utilizador(Utilizador utilizador){
        super(utilizador.getEmail(), utilizador.getPassword(), utilizador.getNome(), utilizador.getGps());
        this.codUtilizador = utilizador.getCodUtilizador();
        this.encomendas_standby = utilizador.getEncomendas_Standy();
        this.encomendas_entregues = utilizador.getEncomendas_entregues();
    }

    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public List<Encomenda> getEncomendas_Standy() {
        return this.encomendas_standby.stream().collect(Collectors.toList());
    }

    public void setEncomendas_Standy(List<Encomenda> encomendas) {
        this.encomendas_standby = encomendas.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public List<Encomenda> getEncomendas_entregues() {
        return this.encomendas_entregues.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public void setEncomendasEntregues(List<Encomenda> encomendas) {
        this.encomendas_entregues = encomendas.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public List<Encomenda> getAllEncomendas(){
        List<Encomenda> res = new ArrayList<>();
        res.addAll(getEncomendas_entregues());
        res.addAll(getEncomendas_Standy());

        return res;
    }
    public void setEncomendas_entregues(List<Encomenda> encomendas) {
        this.encomendas_entregues = encomendas.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public void addToStandby(Encomenda e){
        this.encomendas_standby.add(e);
    }

    public void addEncomendaEntregue(Encomenda encomenda){
        this.encomendas_standby.remove(encomenda);
        this.encomendas_entregues.add(encomenda);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("\nEmail: ").append(this.getEmail());
        sb.append("\nNome: ").append(this.getNome());
        sb.append("\nMorada: ").append(this.getGps());

        return sb.toString();
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }

    public Integer nrEncomendas() {
        return this.encomendas_entregues.size() + this.encomendas_standby.size();
    }
}
