/**
 * classe que representa uma transportadora
 */
package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class Transportadora extends Estafeta implements Serializable {

    private int nif;
    private double taxaKm;
    private double taxaPeso;
    private int numEncomendas;
    private Set<String> rota;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\


    public Transportadora() {
        super();
        this.nif = 0;
        this.taxaKm = 0;
        this.taxaPeso = 0;
        this.numEncomendas = 0;
    }

    public Transportadora(String voluntaryCode, String name, Coordenadas gps, double raio, double velocidade, double numKm, boolean isFree, boolean isMedic, double classificacao,
                          int numCla, int nif, double taxaKm, double taxaPeso, int numEncomendas, boolean occup, List<Notificacao> notificacoes) {
        super(voluntaryCode, name, gps, raio, velocidade, numKm, isFree,isMedic, classificacao,numCla,"Transportadora",occup,notificacoes);
        this.nif = nif;
        this.taxaKm = taxaKm;
        this.taxaPeso = taxaPeso;
        this.numEncomendas = numEncomendas;
        this.rota = new TreeSet<>();
    }

    public Transportadora(Transportadora t) {
        super(t);
        this.nif = t.getNif();
        this.taxaKm = t.getTaxaKm();
        this.taxaPeso = t.getTaxaPeso();
        this.numEncomendas = t.getNumEncomendas();
        setRota(t.getRota());
    }

    //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * devolve nif
     * @return nif
     */
    public int getNif() {
        return nif;
    }

    /**
     * devolve taxa Km
     * @return taxaKm
     */
    public double getTaxaKm() {
        return taxaKm;
    }

    /**
     * devolve taxa de peso
     * @return taxa de peso
     */
    public double getTaxaPeso() {
        return taxaPeso;
    }

    /**
     * devolve maximo de encomendas
     * @return maximo de encomendas
     */
    public int getNumEncomendas() {
        return numEncomendas;
    }

    /**
     * altera maximo de encomendas
     */
    public void setNumEncomendas() {
        this.numEncomendas ++;
    }

    /**
     * devolve rota
     * @return list de encCode
     */
    public Set<String> getRota(){
        return new TreeSet<>(rota);
    }

    /**
     * devolve tamanho da rota
     * @return rotaSize
     */
    public int getRotaSize(){
        return rota.size();
    }

    /**
     * altera rota
     * @param rota list rota
     */
    public void setRota(Set<String> rota){
        this.rota = new TreeSet<>(rota);
    }

    /**
     * adiciona encCode à rota
     * @param rota encCode
     */
    public void addRota(String rota){
        this.rota.add(rota);
    }

    /**
     * remora encCode da rota
     * @param enc encCode
     */
    public void remEncRota(String enc){
        rota.remove(enc);
    }

    /**
     * verifica se a rota contém a encCode
     * @param encCode encCode
     * @return        true se contém encCode
     */
    public boolean containsRota(String encCode) {
        return rota.contains(encCode);
    }

    //--------------------------------------------------------------toString, equals e clone--------------------------------------------------------------------------\\


    public String toString1() {
        final StringBuilder sb = new StringBuilder("Transportadora{");
        sb.append("nif=").append(nif);
        sb.append(", taxaKm=").append(taxaKm);
        sb.append(", taxaPeso=").append(taxaPeso);
        sb.append(", numEncomendas=").append(numEncomendas);
        sb.append('}').append("\n");
        return sb.toString();
    }

    @Override
    public String toString() {
        return super.toString() + toString1();
    }

    public boolean equals1(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        if (!super.equals(o))
            return false;
        Transportadora that = (Transportadora) o;
        return nif == that.nif &&
                Double.compare(that.taxaKm, taxaKm) == 0 &&
                Double.compare(that.taxaPeso, taxaPeso) == 0 &&
                numEncomendas == that.numEncomendas;
    }

        @Override
    public boolean equals(Object o) {
        return super.equals(o) && equals1(o);
    }

    public Transportadora clone() {
        return new Transportadora(this);
    }

    //--------------------------------------------------------------Outros métodos--------------------------------------------------------------------------\\

}
