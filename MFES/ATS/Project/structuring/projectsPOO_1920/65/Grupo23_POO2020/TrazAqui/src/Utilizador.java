import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Classe de utilizador
 */
public class Utilizador implements Serializable {
    /*Código*/
    private String codUtilizador;

    /*Password*/
    private String pwd;

    /*Nome*/
    private String nome;

    /*Posição*/
    private Posicao posicao;

    /*Registo de encomendas*/
    private Map<String, Encomenda> encomendas;

    /*Ofertas de Propostas de transportadoras*/
    private String ofertasTrans;


    /**
     * Construtores
     */

    /**
     * Por omissão
     */
    public Utilizador() {
        this.encomendas = new HashMap<>();
        this.posicao = new Posicao();
        this.nome = null;
        this.codUtilizador = null;
        this.pwd = null;
        this.ofertasTrans = null;
    }

    /**
     * Cópia
     */
    public Utilizador(Utilizador ut) {
        this.encomendas = ut.getEncomendas();
        this.posicao = ut.getPosicao();
        this.nome = ut.getNome();
        this.codUtilizador = ut.getCodUtilizador();
        this.pwd = ut.getPwd();
        this.ofertasTrans = ut.getOfertasT();
    }

    /**
     * Parametrizado
     */
    public Utilizador(Map<String, Encomenda> map, double x, double y, String nome, String codUtilizador) {
        this.encomendas = map;

        this.posicao = new Posicao(x,y);
        this.nome = nome;
        this.codUtilizador = codUtilizador;
        this.pwd = codUtilizador + "23";
        this.ofertasTrans = null;
    }

    /**
     * Getters
     */
    public Posicao getPosicao() {
        return posicao;
    }

    public String getNome() {
        return nome;
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public Map<String, Encomenda> getEncomendas() {
        return encomendas;
    }

    public Encomenda getEncomenda(String codEncomenda){
        return encomendas.get(codEncomenda);
    }

    public String getPwd(){
        return pwd;
    }

    public String getOfertasT(){
        return ofertasTrans;
    }


    /**
     * Setters
     */
    public void setEncomendas(Map<String, Encomenda> encomendas) {
        this.encomendas = encomendas;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setPosicao(Posicao posicao) {
        this.posicao = posicao;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public void setPwd(String pwd){
        this.pwd = pwd;
    }

    public void setOfertasTrans(String l){
        this.ofertasTrans = l;
    }

    /**
     * Adds
     */
    public void addEncomenda(Encomenda e){
        encomendas.put(e.getCodEncomenda(), e);
    }


    /**
     * Manipulação
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Utilizador: ").append(codUtilizador)
                .append("Nome: ").append(nome)
                .append("Posicao: ").append(posicao)
                .append("Encomendas: ").append(encomendas.toString())
                .append("Password: ").append(pwd)
                .append("Ofertas: ").append(ofertasTrans);

        return sb.toString();
    }

    /**
     * Metodo Equals
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return Objects.equals(encomendas, that.encomendas) &&
                Objects.equals(posicao, that.posicao) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(codUtilizador, that.codUtilizador) &&
                Objects.equals(pwd, that.pwd) &&
                Objects.equals(ofertasTrans, that.ofertasTrans);
    }

    /**
     * Metodo Hashcode
     */
    public int hashCode() {
        return Objects.hash(encomendas, posicao, nome, codUtilizador, pwd, ofertasTrans);
    }

    /**
     * Metodo Clone
     */
    public Utilizador clone() {
        return new Utilizador(this);
    }


    /**
     * Funçao que aceita encomenda
     */
    public void aceitaEncomenda(String e){
        encomendas.get(e).setAceite(true);
    }


    /**
     * Funçao que valida se na lista de encomendas existe alguma entregue
     */
    public boolean temEncEntregues(){
        boolean flag = false;

        for (Map.Entry<String, Encomenda> encs : encomendas.entrySet()) {
            if (encs.getValue().getEntregue()) {
                flag = true;
                break;
            }
        }

        return flag;
    }

    /**
     * Funçao que retorna o mapa de encomendas sem Classificaçao
     */
    public Map<String, Encomenda> getEncsSValid(){
        Map<String, Encomenda> m = new HashMap<>();

        for(Map.Entry<String, Encomenda> a : encomendas.entrySet()){
            if(a.getValue().getEntregue() && !a.getValue().getclassificada()) {
                m.put(a.getKey(), a.getValue());
            }
        }

        return m;
    }

    /**
     * Funçao que retorna o número de encomendas entregues de um utilizador
     */
    public int nEncsEntregues(){
        int ret = 0;
        for(Encomenda e : encomendas.values()){
            if (e.getEntregue())
                ret++;
        }
        return ret;
    }

    /**
     * Funçao que retorna a lista de encomendas entregues num periodo de tempo
     */
    public List<Encomenda> encsBetween(LocalDateTime data1, LocalDateTime data2){
        List<Encomenda> ret = new ArrayList<>();

        if(encomendas.size() == 0) return null;

        for(Encomenda e : encomendas.values()){
            LocalDateTime l = e.getDataChegada();
            if(l != null && l.isAfter(data1) && l.isBefore(data2)){
                ret.add(e);
            }
        }
        return ret;
    }

    /**
     * Funçao que retorna a lista de encomendas entregues por voluntarios
     */
    public List<Encomenda> encsEntregueVoluntario(List<Encomenda> l){
        List<Encomenda> ret = new ArrayList<>();

        for(Encomenda e : l){
            if(e.getCodEntregador().charAt(0) == 'v'){
                ret.add(e);
            }
        }

        return ret;
    }

    /**
     * Funçao que retorna a lista de encomendas entregues por Transportadoras
     */
    public List<Encomenda> encsEntregueTransportadora(List<Encomenda> l){
        List<Encomenda> ret = new ArrayList<>();

        for(Encomenda e : l){
            if(e.getCodEntregador().charAt(0) == 't'){
                ret.add(e);
            }
        }

        return ret;
    }

}
