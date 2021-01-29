/**
 * classe que representa um estafeta
 */
package Model;

import java.io.Serializable;
import java.util.*;

public class Estafeta implements Comparable<Estafeta>, Serializable {

    private String code;
    private String name;
    private String type;
    private Coordenadas gps;
    private double raio;
    private double velocidade;
    private double numKm;
    private boolean isFree;
    private boolean isMedic;
    private boolean occup;
    private double classificacao;
    private int numCla;
    private Set<String> encomendas;
    private List<Notificacao> notificacoes;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public Estafeta() {
        this.code = "";
        this.name = "";
        this.type = "";
        this.gps = new Coordenadas();
        this.raio = 0d;
        this.velocidade = 0d;
        this.numKm = 0d;
        this.isFree = false;
        this.isMedic = false;
        this.occup = false;
        this.classificacao = 0d;
        this.numCla = 0;
        this.encomendas = new TreeSet<>();
        this.notificacoes = new ArrayList<>();
    }

    public Estafeta(String voluntaryCode, String name, Coordenadas gps, double raio, double velocidade, double numKm, boolean isFree, boolean isMedic, double classificacao, int numCla, String type,boolean occup, List<Notificacao> notificacoes) {
        this.code = voluntaryCode;
        this.name = name;
        this.type = type;
        this.gps = gps.clone();
        this.raio = raio;
        this.velocidade = velocidade;
        this.numKm = numKm;
        this.classificacao = classificacao;
        this.numCla = numCla;
        this.isFree = isFree;
        this.occup = occup;
        this.isMedic = isMedic;
        this.encomendas = new TreeSet<>();
        this.notificacoes = new ArrayList<>();
    }

    public Estafeta(Estafeta v) {
        this.code = v.getCode();
        this.name = v.getName();
        this.type = v.getType();
        this.gps = v.getGps();
        this.raio = v.getRaio();
        this.velocidade = v.getVelocidade();
        this.numKm = v.getNumKm();
        this.isFree = v.isFree();
        this.isMedic = v.isMedic();
        this.classificacao = v.getClassificacao();
        this.numCla = v.getNumCla();
        setEncomendas(v.getEncomendas());
        setNotificacoes(v.getNotificacoes());
    }

    //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * Devolve código
     *
     * @return Código
     */
    public String getCode() {
        return code;
    }

    /**
     * Devolve tipo de estafeta
     * @return Tipo de estafeta
     */
    public String getType() {
        return type;
    }

    /**
     * Altera o código
     * @param code Código
     */
    public void setCode(String code) {
        this.code = code;
    }

    /**
     * Devolve nome
     * @return Nome
     */
    public String getName() {
        return name;
    }

    /**
     * Devolve gps
     * @return Gps
     */
    public Coordenadas getGps() {
        return gps.clone();
    }

    /**
     * Altera o gps
     * @param gps Gps
     */
    public void setGps(Coordenadas gps) {
        this.gps.setLatitude(gps.getLatitude());
        this.gps.setLongitude(gps.getLongitude());
    }

    /**
     * Devolve raio
     * @return Raio
     */
    public double getRaio() {
        return raio;
    }

    /**
     * Altera o raio
     * @param raio Raio
     */
    public void setRaio(double raio) {
        this.raio = raio;
    }

    /**
     * Devolve a velocidade
     * @return Velocidade
     */
    public double getVelocidade() {
        return velocidade;
    }

    /**
     * Altera a velocidade
     * @param velocidade Velocidade
     */
    public void setVelocidade(double velocidade) {
        this.velocidade = velocidade;
    }

    /**
     * Devolve o número de quilómetros
     * @return Número de quilómetros
     */
    public double getNumKm() {
        return numKm;
    }

    /**
     * Altera o número de quilómetros
     * @param numKm Número de quilómetros
     */
    public void setNumKm(double numKm) {
        this.numKm = numKm;
    }

    /**
     * Atualizar o número de quilómetros
     * @param numKm número de quilómetros
     */
    public void addNumKm(double numKm) {
        this.numKm += numKm;
    }

    /**
     * Indica se está disponível
     * @return True se está disponível, false caso contrário
     */
    public boolean isFree() {
        return isFree;
    }

    /**
     * Alterar o estado de disponibilidade
     * @param free Estado de disponibilidade
     */
    public void setFree(boolean free) {
        isFree = free;
    }

    /**
     * Indica se transporta encomendas médicas
     * @return True se transporta encomendas médicas, false caso contrário
     */
    public boolean isMedic() {
        return isMedic;
    }

    /**
     * Altera a informação que indica se o estafeta transporta encomendas médicas
     * @param medic True ou false se transporta ou não encomendas médicas, respetivamente
     */
    public void setMedic(boolean medic) {
        isMedic = medic;
    }

    /**
     * Devolve a classificação
     * @return Classificação
     */
    public double getClassificacao() {
        return classificacao;
    }

    /**
     * Altera a classificação
     * @param classificacao Classificação
     */
    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    /**
     * Devolve o número de classificações
     * @return Número de classificações
     */
    public int getNumCla() {
        return numCla;
    }

    /**
     * Incrementa o número de classificações
     */
    public void incNumCla() {
        this.numCla ++;
    }

    /**
     * Retorna a lista dos registos
     * @return Lista de registos
     */
    public List<String> getEncomendas() {
        return new ArrayList<>(encomendas);
    }

    /**
     * Altera a lista de registos
     * @param enc Lista de registos
     */
    public void setEncomendas(List<String> enc) {
        this.encomendas = new TreeSet<>(enc);
    }

    /**
     * Adicionar uma encomenda aos registos e incrementa o número de encomendas
     * @param enc Código de encomenda
     */
    public void setEnc(String enc) {
        this.encomendas.add(enc);
        if(this.getType().equals("Transportadora"))
            ((Transportadora)this).setNumEncomendas();
    }

    /**
     * Remove uma encomenda dos registos
     * @param encCode Código de encomenda
     */
    public void removeEnc(String encCode){
        encomendas.remove(encCode);
    }

    /**
     * Retorna um booleano que indica se o estafeta está ocupado ou não
     * @return occup
     */
    public boolean isOccup() {
        return occup;
    }

    /**
     * Altera a informação que indica se está ocupado
     * @param occup Booleano que indica se está ocupado
     */
    public void setOccup(boolean occup) {
        this.occup = occup;
    }

    /**
     * Altera o tipo de estafeta
     * @param type Tipo
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Devolve a lista de notificações
     * @return Lista de notificações
     */
    public List<Notificacao> getNotificacoes() {
        return notificacoes;
    }

    /**
     * Altera a lista de notificações
     * @param notificacoes Lista de notificações
     */
    public void setNotificacoes(List<Notificacao> notificacoes) {
        this.notificacoes = new ArrayList<>();
        this.notificacoes.addAll(notificacoes);
    }

    /**
     * Devolve o número de notificações
     * @return Número de notificações
     */
    public int getNumNotificacoes() {
        return notificacoes.size();
    }

    /**
     * Adiciona uma notificação
     * @param not       Conteúdo da notificação
     * @param type      Tipo de notificação
     * @param estCode   Código de estafeta
     */
    public void addNotificacao(String not, int type, String estCode) {
        notificacoes.add(new Notificacao(not, type, estCode));
    }

    /**
     * Remove um notificação
     * @param not Conteúdo de notificação
     */
    public void removeNotificacao(String not) {
        notificacoes.remove(not);
    }

    //--------------------------------------------------------------toString, equals e clone--------------------------------------------------------------------------\\

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(type);
        sb.append("{estafetaCode='").append(code).append('\'');
        sb.append(", name='").append(name).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", raio=").append(raio);
        sb.append(", velocidade=").append(velocidade);
        sb.append(", isFree=").append(isFree);
        sb.append(", isMedic=").append(isMedic);
        sb.append(", classificacao=").append(classificacao);
        sb.append(", \nregisterV=").append(encomendas);
        sb.append('}').append("\n");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Estafeta that = (Estafeta) o;
        return Double.compare(that.raio, raio) == 0 &&
                Double.compare(that.velocidade, velocidade) == 0 &&
                isFree == that.isFree &&
                isMedic == that.isMedic &&
                Double.compare(that.classificacao, classificacao) == 0 &&
                Objects.equals(code, that.code) &&
                Objects.equals(name, that.name) &&
                Objects.equals(gps, that.gps) &&
                Objects.equals(encomendas, that.encomendas);
    }

    public Estafeta clone() {
        return new Estafeta(this);
    }

    public int compareTo(Estafeta e) {
        if(this.numKm > e.getNumKm())
            return -1;
        else if(this.numKm < e.getNumKm())
            return 1;
        else
            return 0;
    }

    //--------------------------------------------------------------Outros métodos--------------------------------------------------------------------------\\

    /**
     * Atualiza a classificação
     * @param classificacao Classificação
     */
    public void atualizaClassificacao(double classificacao) {
        setClassificacao((getClassificacao() * getNumCla() + classificacao) / (getNumCla() + 1));
        incNumCla();
    }

    /**
     * Adiciona uma encomenda
     * @param encCode Código de encomenda
     */
    public void addEncomenda(String encCode) {
        encomendas.add(encCode);
    }

    /**
     * Devolve um booleano que indica se os registos contém uma encomenda
     * @param encCode   Código da encomenda
     * @return          True se contém a encomenda, false caso contrário
     */
    public boolean containsEncomenda(String encCode) {
        return encomendas.contains(encCode);
    }

    /**
     * Limpa a lista de notificações
     */
    public void limpaNotificacoes() {
        notificacoes = new ArrayList<>();
    }
}
