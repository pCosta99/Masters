package Modelo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Classe abstrata que contém a implementação da estrutura da Transportadora.
 */
public class Utilizador implements Serializable {
    private String codUti;
    private String nome;
    private Coordenadas gps;
    private int nEncomendas;
    private List<Encomenda> pendentes;
    private List<String> transportadoraEntregue;

    // --------------------------- Constructor -------------------------

    /**
     * Construtor por omissão
     */
    public Utilizador(){
        this.codUti = "";
        this.nome = "";
        this.gps = new Coordenadas();
        this.nEncomendas = 0;
        this.pendentes = new ArrayList<>();
        this.transportadoraEntregue = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param codUti                            Codigo
     * @param nome                              Nome
     * @param gps                               Coordenadas
     * @param nE                                Numero de encomendas feitas
     * @param enc                               Lista de encomendas pendentes
     * @param transportadoraEntregue            Lista de encomendas entregues
     */
    public Utilizador(String codUti, String nome, Coordenadas gps,int nE,List<Encomenda> enc,List<String> transportadoraEntregue) {
        this.codUti = codUti;
        this.nome = nome;
        this.gps = gps;
        this.nEncomendas = nE;
        this.pendentes = enc;
        this.transportadoraEntregue = transportadoraEntregue;
    }

    /**
     * Construtor por cópia
     * @param u             Utilizador
     */
    public Utilizador (Utilizador u){
        this.codUti = u.getCodUti();
        this.nome = u.getNome();
        this.gps = u.getGps();
        this.nEncomendas = u.getnEncomendas();
        setPendentes(u.getPendentes());
        setTransportadoraEntregue(u.getTransportadoraEntregue());
    }

    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve o código de utilizador
     * @return String
     */
    public String getCodUti() {
        return codUti;
    }

    /**
     * Define o codigo do utilizador
     * @param codUti
     */
    public void setCodUti(String codUti) {
        this.codUti = codUti;
    }

    /**
     * Devolve o nome do utilizador
     * @return String
     */
    public String getNome() {
        return nome;
    }

    /**
     * Define o nome do utilizador
     * @param nome              Nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Devolve as coordenadas gps
     * @return Coordenadas
     */
    public Coordenadas getGps() {
        return gps.clone();
    }

    /**
     * Define as coordenadas gps
     * @param gps           Coordenadas
     */
    public void setGps(Coordenadas gps) {
        this.gps.setLatitude(gps.getLatitude());
        this.gps.setLongitude(gps.getLongitude());
    }

    /**
     * Devolve o numero de encomendas
     * @return int
     */
    public int getnEncomendas() {
        return nEncomendas;
    }


    /**
     * Devolve uma lista de encomendas pendentes do utilizador
     * @return List<Encomenda>
     */
    public List<Encomenda> getPendentes() {
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda e: this.pendentes)
            aux.add(e);
        return aux;
    }

    /**
     * Define uma lista de encomendas pendentes do utilizador
     * @param pendentes                 Lista de encomenda pendentes
     */
    public void setPendentes(List<Encomenda> pendentes) {
        this.pendentes = new ArrayList<>();
        for(Encomenda s: pendentes)
            this.pendentes.add(s);
    }

    /**
     * Devolve um lista do que foi entregue pela transportadora.
     * @return List<String>
     */
    public List<String> getTransportadoraEntregue() {
        List<String> aux = new ArrayList<>();
        for(String s: this.transportadoraEntregue)
            aux.add(s);
        return aux;
    }

    /**
     * Define um lista do que foi entregue pela transportadora
     * @param transportadoraEntregue                Lista de encomendas entregues
     */
    public void setTransportadoraEntregue(List<String> transportadoraEntregue) {
        this.transportadoraEntregue = new ArrayList<>();
        this.transportadoraEntregue.addAll(transportadoraEntregue);
    }

    // --------------------------- Auxiliaries -------------------------

    /**
     * Devolve uma cópia da instância
     * @return Utilizador               this
     */
    public Utilizador clone(){
        return new Utilizador(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o          Objeto a comparar
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Utilizador)) return false;
        Utilizador that = (Utilizador) o;
        return getnEncomendas() == that.getnEncomendas() &&
                Objects.equals(getCodUti(), that.getCodUti()) &&
                Objects.equals(getNome(), that.getNome()) &&
                Objects.equals(getGps(), that.getGps()) &&
                Objects.equals(getPendentes(), that.getPendentes()) &&
                Objects.equals(getTransportadoraEntregue(), that.getTransportadoraEntregue());
    }

    /**
     * Método hashCode do objeto
     * @return hash do objeto
     */
    @Override
    public int hashCode() {
        return Objects.hash(getCodUti(), getNome(), getGps(), getnEncomendas(), getPendentes(), getTransportadoraEntregue());
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Utilizador{");
        sb.append("codUti='").append(codUti).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", nEncomendas=").append(nEncomendas);
        sb.append(", pendentes=").append(pendentes);
        sb.append(", transportadoraEntregue='").append(transportadoraEntregue).append('\'');
        sb.append('}');
        return sb.toString();
    }

    /**
     * Função que adiciona uma encomenda aos pendentes
     * @param e                     Encomenda
     */
    public void adicionaPendentes(Encomenda e){
        this.pendentes.add(e);
    }

    /**
     * Função que remove uma encomenda dos pendentes
     * @param e                     Encomenda
     */
    public void removePendente(Encomenda e){
        this.pendentes.remove(e);
    }

    /**
     * Função que devolve uma encomenda pendente dado um código
     * @param codEncomenda          Codigo da encomenda
     * @return Encomenda
     */
    public Encomenda devolvePendente(String codEncomenda){
        for(Encomenda e: this.pendentes){
            if(e.getCodEncomenda().equals(codEncomenda))
                return e;
        }
        return null;
    }

    /**
     * Função que incrementa o numero de encomendas
     */
    public void incrementaNencomendas(){
        this.nEncomendas++;
    }

    /**
     * Função que compara o numero de encomendas dado um utilizador
     * @param u                        Utilizador
     * @return int
     */
    public int compareNum(Utilizador u){
        if(this.nEncomendas == u.getnEncomendas()) return 0;
        else if((this.nEncomendas - u.getnEncomendas()) > 0) return -1;
        return 1;
    }

    /**
     * Função que adiciona uma encomenda à lista das entregues
     * @param e                        Encomenda
     */
    public void adicionaEncomendas(Encomenda e){
        this.transportadoraEntregue.add(e.getCodEncomenda());
    }

}


