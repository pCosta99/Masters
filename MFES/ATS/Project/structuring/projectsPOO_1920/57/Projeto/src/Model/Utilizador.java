/**
 * classe que representa um utilizador
 */
package Model;

import java.io.Serializable;
import java.util.*;

public class Utilizador implements Comparable<Utilizador>, Serializable {
    private String codigoUtilizador;
    private String nome;
    private Coordenadas gps;
    private Set<String> encomendas;
    private Set<String> standBy;
    private List<Notificacao> notificacoes;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public Utilizador() {
        this.codigoUtilizador = "";
        this.nome = "";
        this.gps = new Coordenadas();
        this.encomendas = new TreeSet<>();
        this.standBy = new TreeSet<>();
        this.notificacoes = new ArrayList<>();
    }

    public Utilizador(String codigoUtilizador, String nome, Coordenadas gps, Set<String> encomendas, Set<String> standBy, List<Notificacao> notificacoes) {
        this.codigoUtilizador = codigoUtilizador;
        this.nome = nome;
        this.gps = gps.clone();
        setEncomendas(encomendas);
        setStandBy(standBy);
        setNotificacoes(notificacoes);
    }

    public Utilizador(Utilizador user) {
        this.codigoUtilizador = user.getCodigoUtilizador();
        this.nome = user.getName();
        this.gps = user.getGps();
        setEncomendas(user.getEncomendas());
        setStandBy(user.getStandBy());
        setNotificacoes(user.getNotificacoes());
    }

     //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * devolve userCod
     * @return userCod
     */
    public String getCodigoUtilizador() {
        return codigoUtilizador;
    }

    /**
     * devolve name
     * @return name
     */
    public String getName() {
        return nome;
    }

    /**
     * devolve gps
     * @return gps
     */
    public Coordenadas getGps() {
        return gps.clone();
    }

    /**
     * altera liste de entregas
     * @param enc set encCode
     */
    public void setEncomendas(Set<String> enc) {
        this.encomendas = new TreeSet<>();
        this.encomendas.addAll(enc);
    }

    /**
     * devolve lista de entregas
     * @return set de encCode
     */
    public Set<String> getEncomendas(){
        return new TreeSet<>(encomendas);
    }

    /**
     * devolve entregas.size
     * @return size
     */
    public int getEntregasSize(){
        return encomendas.size();
    }

    /**
     * devolve nome
     * @return nome
     */
    public String getNome() {
        return nome;
    }

    /**
     * altera nome
     * @param nome nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * devolve codigos de encomendas em standBy
     * @return list encCode
     */
    public Set<String> getStandBy() {
        return standBy;
    }

    /**
     * altera list standBy
     * @param standBy list de encCode
     */
    public void setStandBy(Set<String> standBy) {
        this.standBy = new TreeSet<>();
        this.standBy.addAll(standBy);
    }

    /**
     * adiciona encCode à lista de standBy
     * @param encCode encCode
     */
    public void addStandBy(String encCode) {
        standBy.add(encCode);
    }

    /**
     * remove encCode do standBy
     * @param encCode encCode
     */
    public void removeStandBy(String encCode) {
        standBy.remove(encCode);
    }

    /**
     * devolve list de notificações
     * @return list de notificações
     */
    public List<Notificacao> getNotificacoes() {
        return notificacoes;
    }

    /**
     * altera list de notificações
     * @param notificacoes list de notificações
     */
    public void setNotificacoes(List<Notificacao> notificacoes) {
        this.notificacoes = new ArrayList<>();
        this.notificacoes.addAll(notificacoes);
    }

    /**
     * devolve numero de notificações
     * @return numero
     */
    public int getNumNotificacoes() {
        return notificacoes.size();
    }

    /**
     * adicionar notificação
     * @param not       conteúdo da notificação
     * @param type      type
     * @param estCode   estafeta code
     */
    public void addNotificacao(String not, int type, String estCode) {
        notificacoes.add(new Notificacao(not, type, estCode));
    }

    //--------------------------------------------------------------toString, equals e clone--------------------------------------------------------------------------\\


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Utilizador{");
        sb.append("codigoUtilizador='").append(codigoUtilizador).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", gps='").append(gps).append('\'');
        sb.append(", entregas=").append(encomendas);
        sb.append('}').append("\n");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return Objects.equals(codigoUtilizador, that.codigoUtilizador) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(gps, that.gps) &&
                Objects.equals(encomendas, that.encomendas);
    }

    public Utilizador clone() {
        return new Utilizador(this);
    }

    public int compareTo(Utilizador u) {
        return u.getEncomendas().size() - this.encomendas.size();
    }

    //--------------------------------------------------------------Outros métodos--------------------------------------------------------------------------\\

    /**
     * adicionar encomenda entregue
     * @param enc encCode
     */
    public void addEncomenda(String enc) {
        this.encomendas.add(enc);
    }

    /**
     * remover encomenda entregue
     * @param enc encCode
     */
    public void removeEncomenda(String enc) {
        this.encomendas.remove(enc);
    }

    /**
     * limpa notificações
     */
    public void limpaNotificacoes() {
        notificacoes = new ArrayList<>();
    }

    /**
     * verifica se a encomenda está em standBy
     * @param enc encCode
     * @return    true se estiver em standBy
     */
    public boolean isEncStandBy(String enc){
        return standBy.contains(enc);
    }
}
