package Modelo;

import java.io.Serializable;
import java.util.Objects;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe que contém a implementação da estrutura de uma Encomenda
 */
public class Encomenda  implements Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private boolean entregue;
    private double tempo;
    private boolean medica;
    private List<LinhaEncomenda> e;


    // --------------------------- Constructor -------------------------

    /**
     * Construtor por omissão
     */
    public Encomenda (){
        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.entregue = false;
        this.tempo = 0;
        this.medica = false;
        this.e = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param codEncomenda                      Codigo
     * @param codUtilizador                     Codigo utilizador
     * @param codLoja                           Codigo loja
     * @param peso                              Peso  encomenda
     * @param entregue                          Entregue ou não
     * @param tempo                             Tempo
     * @param medica                            Médica ou não
     * @param e                                 Lista de linha de encomendas
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso,boolean entregue,double tempo,boolean medica, List<LinhaEncomenda> e) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.entregue = entregue;
        this.tempo = tempo;
        this.medica = medica;
        this.setE(e);
    }

    /**
     * Construtor por cópia
     * @param e             Encomenda
     */
    public Encomenda (Encomenda e){
        this.codEncomenda = e.getCodEncomenda();
        this.codUtilizador = e.getCodUtilizador();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.entregue = e.isEntregue();
        this.tempo = e.getTempo();
        setE(e.getE());
        this.medica = e.isMedica();
    }


    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve um código de encomenda
     * @return String
     */
    public String getCodEncomenda() {
        return codEncomenda;
    }

    /**
     * Devolve um código de utilizador
     * @return String
     */
    public String getCodUtilizador() {
        return codUtilizador;
    }

    /**
     * Devolve um código de loja
     * @return String
     */
    public String getCodLoja() {
        return codLoja;
    }

    /**
     * Devolve o peso da encomenda
     * @return double
     */
    public double getPeso() {
        return peso;
    }

    /**
     * Devolve uma lista de Linhas de encomendas
     * @return List<LinhaEncomenda>
     */
    public List<LinhaEncomenda> getE() {
        ArrayList<LinhaEncomenda> aux = new ArrayList<>();
        for(LinhaEncomenda l: this.e){
            aux.add(l.clone());
        }
        return aux;
    }

    /**
     * Define a lista de linhas de encomenda
     * @param e             Lista Linha de encomenda
     */
    public void setE(List<LinhaEncomenda> e) {
        this.e = new ArrayList<>();
        for(LinhaEncomenda l: e){
            this.e.add(l.clone());
        }
    }

    /**
     * Devolve o tempo da encomenda
     * @return double
     */
    public double getTempo() {
        return tempo;
    }

    /**
     * Define o tempo da encomenda
     * @param tempo     Tempo
     */
    public void setTempo(double tempo) {
        this.tempo = tempo;
    }

    /**
     * Devolve um boolean. Se é uma encomenda médica ou não
     * @return boolean
     */
    public boolean isMedica() {
        return medica;
    }

    /**
     * Devolve um boolean. Se foi entregue ou não
     * @return boolean
     */
    public boolean isEntregue() {
        return entregue;
    }

    /**
     * Define se foi entregue ou não.
     * @param entregue          Entregue ou não
     */
    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    // --------------------------- Auxiliaries -------------------------

    /**
     * Devolve uma cópia da instância
     * @return Encomenda
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }

    /**
     * Verifica a igualdade com outro objeto.
     * @param o          Objeto a comparar
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Encomenda)) return false;
        Encomenda encomenda = (Encomenda) o;
        return Double.compare(encomenda.getPeso(), getPeso()) == 0 &&
                isEntregue() == encomenda.isEntregue() &&
                Double.compare(encomenda.getTempo(), getTempo()) == 0 &&
                isMedica() == encomenda.isMedica() &&
                Objects.equals(getCodEncomenda(), encomenda.getCodEncomenda()) &&
                Objects.equals(getCodUtilizador(), encomenda.getCodUtilizador()) &&
                Objects.equals(getCodLoja(), encomenda.getCodLoja()) &&
                Objects.equals(getE(), encomenda.getE());
    }

    /**
     * Método hashCode do objeto
     * @return hash do objeto
     */
    @Override
    public int hashCode() {
        return Objects.hash(getCodEncomenda(), getCodUtilizador(), getCodLoja(), getPeso(), isEntregue(), getTempo(), isMedica(), getE());
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Encomenda{");
        sb.append("codEncomenda='").append(codEncomenda).append('\'');
        sb.append(", codUtilizador='").append(codUtilizador).append('\'');
        sb.append(", codLoja='").append(codLoja).append('\'');
        sb.append(", peso=").append(peso);
        sb.append(", entregue=").append(entregue);
        sb.append(", tempo=").append(tempo);
        sb.append(", medica=").append(medica);
        sb.append(", e=").append(e);
        sb.append('}');
        return sb.toString();
    }
}


