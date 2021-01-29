package Model;

import Exceptions.EncomendaInexistenteException;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que define um transporte
 */
public class Transporte extends LocalCodeName implements Serializable {

    private Double raio;
    private boolean disp;
    private Map<Utilizador, Set<Encomenda>> encEntregues;
    //Encomendas ainda não levantadas
    private Map<String,Encomenda> futurasEnc;
    private List<Double> classificacoes;
    private boolean encMedicas;

    /**
     * Construtor da classe
     */
    public Transporte(){
        super();
        raio = 0.0;
        disp = true;
        encEntregues = new HashMap<>();
        futurasEnc = new HashMap<>();
        classificacoes = new ArrayList<>();
        encMedicas = false;
    }

    /**
     * Construtor da classe
     * @param code O código do transporte
     * @param nome O nome do transporte
     * @param gps O GPS do transporte
     * @param raio O raio de ação do transporte
     * @param disp As encomendas entregues pelo transporte
     * @param encEntregues As encomendas já entregues pelo transporte
     * @param futurasEnc As encomendas que o transporte tem para ir levantar às lojas
     * @param classificacoes As classificações atribuídas ao transporte
     * @param encMedicas A indicação do transporte ou não de encomendas médicas
     */
    public Transporte(String code, String nome, GPS gps, Double raio, boolean disp, Map<Utilizador, Set<Encomenda>> encEntregues, Map<String, Encomenda> futurasEnc, List<Double> classificacoes, boolean encMedicas) {
        super(code, nome, gps);
        this.raio = raio;
        this.disp = disp;
        setEncEntregues(encEntregues);
        setClassificacoes(classificacoes);
        setFuturasEnc(futurasEnc);
        this.encMedicas = encMedicas;
    }

    /**
     * Construtor da classe
     * @param t O transporte do qual se pretende extrair as informações
     */
    public Transporte(Transporte t){
        super(t);
        setRaio(t.raio);
        disp = t.disp;
        setEncEntregues(t.encEntregues);
        setClassificacoes(t.classificacoes);
        setFuturasEnc(t.futurasEnc);
        encMedicas = t.aceitoTransporteMedicamentos();
    }

    /**
     * Indica se o transporte realiza entregas de medicamentos
     * @return True caso afirmativo e false caso contrário
     */
    public boolean aceitoTransporteMedicamentos(){
        return this.encMedicas;
    }

    /**
     * Altera o estado de aceitação de encomendas de medicamentos
     * @param state O estado para o qual se pretende mudar
     */
    public void aceitaMedicamentos(boolean state){
        this.encMedicas = state;
    }

    /**
     * Define um raio
     * @param raio O raio que se pretende definir
     */
    public void setRaio(Double raio) {
        this.raio = raio;
    }

    /**
     * Troca a disponibilidade do transporte
     */
    public void trocaDisp(){
        disp = !disp;
    }

    /**
     * Indica o raio de um dado transporte
     * @return O raio do transporte
     */
    public Double getRaio() {
        return raio;
    }

    /**
     * Indica a disponibilidade
     * @return True caso afirmativo e caso contrário false
     */
    public boolean isDisp() {
        return disp;
    }

    /**
     * Cria um clone de um conjunto de encomendas
     * @param set O conjunto que se pretende clonar
     * @return O clone do conjunto de encomendas
     */
    private Set<Encomenda> cloneSet(Set<Encomenda> set){
        if(set != null)
            return set.stream().map(Encomenda::clone).collect(Collectors.toSet());
        else
            return null;
    }

    /**
     * Indica as encomendas entregas de um dado transporte
     * @return As encomendas entregues de um dado transporte
     */
    public Map<Utilizador, Set<Encomenda>> getEncEntregues() throws EncomendaInexistenteException {
        if(encEntregues == null) {
            throw new EncomendaInexistenteException("Ainda não entregou encomendas.");
        }
        return this.encEntregues.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, p -> cloneSet(p.getValue())));
    }

    /**
     * Define as encomendas entregues
     * @param encEntregues As encomendas que se pretendem definir
     */
    public void setEncEntregues(Map<Utilizador, Set<Encomenda>> encEntregues) {
        if(encEntregues != null) {
            if(this.encEntregues == null) {
                this.encEntregues = new HashMap<>();
            }
            for (Map.Entry<Utilizador, Set<Encomenda>> ent : encEntregues.entrySet()) {
                this.encEntregues.put(ent.getKey(), cloneSet(ent.getValue()));
            }
        }else {
            this.encEntregues = null;
        }

    }

    /**
     * Indica as encomendas ainda não levantadas
     * @return As encomendas ainda não levantadas
     */
    public Map<String, Encomenda> getFuturasEnc() {
        if(futurasEnc != null)
            return futurasEnc.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey,e -> e.getValue().clone()));
        else return null;
    }

    /**
     * Define as encomendas ainda não levantadas
     * @param futurasEnc As encomendas ainda não levantadas a definir
     */
    public void setFuturasEnc(Map<String, Encomenda> futurasEnc) {
        if(futurasEnc != null)
            this.futurasEnc = futurasEnc.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey,e -> e.getValue().clone()));
        else this.futurasEnc = null;
    }

    /**
     * Adiciona uma encomenda a levantar
     * @param e A encomenda a levantar
     */
    public void addFuturaEnc(Encomenda e) {
        if(futurasEnc == null)
            futurasEnc = new HashMap<>();
        futurasEnc.put(e.getNumEnc(),e.clone());
    }

    /**
     * Remove uma encomenda a levantar
     * @param s O código da encomenda a levantar
     */
    public void removeFuturaEnc(String s) {
        if(futurasEnc != null)
            futurasEnc.remove(s);
    }

    /**
     * Cria um clone de um transporte
     * @return O clone do transporte
     */
    public Transporte clone(){
        return new Transporte(this);
    }

    /**
     * Indica se um objeto é igual a um transporte
     * @param o O objeto com o qual se pretende comparar
     * @return True caso afirmativo e caso contrário false
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Transporte)) return false;
        Transporte that = (Transporte) o;
        return this.getCode().equals(that.getCode());
    }

    /**
     * Indica as classificações de um transporte
     * @return As classificações de um transporte
     */
    public List<Double> getClassificacoes() {
        return new ArrayList<>(classificacoes);
    }

    /**
     * Define as classificações de um transporte
     * @param classificacoes As classificações de um transporte a definir
     */
    public void setClassificacoes(List<Double> classificacoes) {
        this.classificacoes = new ArrayList<>(classificacoes);
    }

    /**
     * Adiciona uma classificação a um transporte
     * @param rate A classificação a ser adicionada
     */
    public void addRate(double rate){
        if(classificacoes == null)
            classificacoes = new ArrayList<>();
        this.classificacoes.add(rate);}

    /**
     * Adiciona encomenda às encomendas entregues
     * @param util utilizador
     * @param enc encomenda
     */
    public void addEncEntregue(Utilizador util, Encomenda enc){
        if(encEntregues == null)
            encEntregues = new HashMap<>();
        if(!encEntregues.containsKey(util)){
            Set<Encomenda> temp = new HashSet<>();
            temp.add(enc);
            encEntregues.put(util,temp);
        }else{
            encEntregues.get(util).add(enc);
        }
    }

    /**
     * Indica as coordenadas GPS dos locais onde o transporte entregou encomendas
     * @return Uma lista das coordenadas GPS
     */
    public List<GPS> extratoDeViagem(){

        List<GPS> res = new ArrayList<>();

        for(Map.Entry<Utilizador, Set<Encomenda>> aux : this.encEntregues.entrySet()){
            res.add(aux.getKey().getGps().clone());
        }

        return res;

    }

    /**
     * Transforma um transporte numa String
     * @return A String correspondente ao transporte
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Nome: ").append(this.getNome()).append("\n")
                .append("Código: ").append(this.getCode()).append("\n")
                .append(this.getGps()).append("\n")
                .append("Encomendas feitas: ").append(encEntregues.size());

        return sb.toString();
    }

    /**
     * Transforma um transporte num hashCode
     * @return hashCode
     */
    public int hashCode() {
        return Objects.hash(getCode(), isDisp(), encMedicas);
    }
}
