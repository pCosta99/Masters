package Modelo;

import java.io.Serializable;
import java.util.*;

/**
 * Classe que contém a implementação da estrutura Loja
 */
public class Loja implements Serializable {

    private String codLoja;
    private String nomeLoja;
    private double tempoAtend;
    private Coordenadas gps;
    private List<Encomenda> registoU;
    private boolean temTempo;
    private double tempoFila;
    private List<Encomenda> pendentes;
    private Map<String,LinhaEncomenda> catalogo;


    // --------------------------- Constructor -------------------------

    /**
     * Construtor por omissão
     */
    public Loja(){
        this.codLoja = "";
        this.nomeLoja = "";
        this.tempoAtend = 0;
        this.gps = new Coordenadas();
        this.registoU = new ArrayList<>();
        this.temTempo = false;
        this.tempoFila = 0;
        this.pendentes = new ArrayList<>();
        this.catalogo = new HashMap<>();
    }

    /**
     * Construtor parametrizado
     * @param codLoja                   Codigo
     * @param nomeLoja                  Nome
     * @param tempo                     Tempo
     * @param gps                       Coordenadas
     * @param registoU                  Registos
     * @param tempoFila                 Tempo de fila
     * @param temTempo                  Se tem ou não tempo
     * @param pendente                  Pendentes
     * @param catalogo                  Catalogo
     */
    public Loja(String codLoja, String nomeLoja, double tempo,Coordenadas gps,List<Encomenda> registoU,double tempoFila,boolean temTempo,List<Encomenda> pendente,Map<String,LinhaEncomenda> catalogo) {
        this.codLoja = codLoja;
        this.nomeLoja = nomeLoja;
        this.tempoAtend = tempo;
        this.gps = gps;
        setRegistoU(registoU);
        this.tempoFila = tempoFila;
        this.temTempo = temTempo;
        setPendentes(pendente);
        setCatalogo(catalogo);
    }

    /**
     * Construtor por cópia
     * @param l         Loja
     */
    public Loja(Loja l){
        this.codLoja = l.getCodLoja();
        this.nomeLoja = l.getNomeLoja();
        this.tempoAtend = l.getTempoAtend();
        this.gps = l.getGps();
        setRegistoU(l.getRegistoU());
        this.tempoFila = l.getTempoFila();
        this.temTempo = l.isTemTempo();
        setPendentes(l.getPendentes());
        setCatalogo(l.getCatalogo());
    }

    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve o codigo da loja
     * @return String
     */
    public String getCodLoja() {
        return codLoja;
    }


    /**
     * Devolve o nome da loja
     * @return String
     */
    public String getNomeLoja() {
        return nomeLoja;
    }

    /**
     * Devolve o tempo de atendimento
     * @return double
     */
    public double getTempoAtend() {
        return tempoAtend;
    }

    /**
     * Devolve uma classe Coordenadas
     * @return Coordenadas
     */
    public Coordenadas getGps() {
        return gps;
    }

    /**
     * Devolve uma lista de encomendas. Registo de encomendas efetuadas
     * @return List<Encomenda>
     */
    public List<Encomenda> getRegistoU() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for(Encomenda e: this.registoU){
            aux.add(e.clone());
        }
        return aux;
    }

    /**
     * Define a lista de encomendas.
     * @param registoU              Lista de registos
     */
    public void setRegistoU(List<Encomenda> registoU) {
        this.registoU = new ArrayList<>();
        for(Encomenda e: registoU){
            this.registoU.add(e.clone());
        }
    }

    /**
     * Devolve um boolean. Se tem ou não informação de fila
     * @return boolean
     */
    public boolean isTemTempo() {
        return temTempo;
    }

    /**
     * Devolve tempo de fila
     * @return double
     */
    public double getTempoFila() {
        return tempoFila;
    }

    /**
     * Devolve lista de encomendas pendentes.
     * @return List<Encomenda>
     */
    public List<Encomenda> getPendentes() {
        return new ArrayList<>(this.pendentes);
    }

    /**
     * Define a lista de encomendas pendentes.
     * @param pendentes             Lista de pendentes
     */
    public void setPendentes(List<Encomenda> pendentes) {
        this.pendentes = new ArrayList<>();
        for(Encomenda e: pendentes)
            this.pendentes.add(e);
    }

    /**
     * Devolve o catalogo de produtos da loja
     * @return Map<String,LinhaEncomenda>
     */
    public Map<String, LinhaEncomenda> getCatalogo() {
        Map<String,LinhaEncomenda> aux = new HashMap<>();
        for(LinhaEncomenda l: this.catalogo.values())
            aux.put(l.getCodProduto(),l);
        return aux;
    }

    /**
     * Define catalogo de produtos
     * @param catalogo          Map de produtos
     */
    public void setCatalogo(Map<String, LinhaEncomenda> catalogo) {
        this.catalogo = new HashMap<>();
        for(LinhaEncomenda l: catalogo.values()){
            this.catalogo.put(l.getCodProduto(),l);
        }
    }

    // --------------------------- Auxiliaries -------------------------

    /**
     * Devolve uma cópia da instância
     * @return Loja                 this
     */
    public Loja clone(){
        return new Loja(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o          Objeto a comparar
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Loja)) return false;
        Loja loja = (Loja) o;
        return Double.compare(loja.getTempoAtend(), getTempoAtend()) == 0 &&
                isTemTempo() == loja.isTemTempo() &&
                Double.compare(loja.getTempoFila(), getTempoFila()) == 0 &&
                Objects.equals(getCodLoja(), loja.getCodLoja()) &&
                Objects.equals(getNomeLoja(), loja.getNomeLoja()) &&
                Objects.equals(getGps(), loja.getGps()) &&
                Objects.equals(getRegistoU(), loja.getRegistoU()) &&
                Objects.equals(getPendentes(), loja.getPendentes()) &&
                Objects.equals(getCatalogo(), loja.getCatalogo());
    }

    /**
     * Método hashCode do objeto
     * @return hash do objeto
     */
    @Override
    public int hashCode() {
        return Objects.hash(getCodLoja(), getNomeLoja(), getTempoAtend(), getGps(), getRegistoU(), isTemTempo(), getTempoFila(), getPendentes(), getCatalogo());
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Modelo.Loja{");
        sb.append("codLoja='").append(codLoja).append('\'');
        sb.append(", nomeLoja='").append(nomeLoja).append('\'');
        sb.append(", tempoAtend=").append(tempoAtend);
        sb.append(", gps=").append(gps);
        sb.append(", registoU=").append(registoU);
        sb.append(", temTempo=").append(temTempo);
        sb.append(", tempoFila=").append(tempoFila);
        sb.append(", pendentes=").append(pendentes);
        sb.append(", catalogo=").append(catalogo);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Adiciona uma encomenda à lista pendentes
     * @param e        Encomenda a adicionar
     */
    public void adicionaPendente(Encomenda e){
        this.pendentes.add(e);
    }

    /**
     * Remove uma encomenda à lista pendentes
     * @param e        Encomenda a remover
     */
    public void removePendente(Encomenda e){
        this.pendentes.remove(e);
    }

    /**
     * Adiciona uma encomenda aos registos.
     * @param e         Encomenda a adiconar
     */
    public void adicionaRegistoUtilizador(Encomenda e){
        this.registoU.add(e);
    }

    /**
     * Devolve Encomenda correspondente ao codEncomenda
     * @param codEncomenda          Codigo da encomenda desejada
     * @return Encomenda            Correspondente
     */
    public Encomenda devolvePendente(String codEncomenda){
        for(Encomenda e: this.pendentes){
            if(e.getCodEncomenda().equals(codEncomenda))
                return e;
        }
        return null;
    }

    /**
     * Adicionar nova linha de encomenda ao catalogo
     * @param l          Linha de encomenda a adicionar
     */
    public void adicionaMapCatalogo(LinhaEncomenda l){
        this.catalogo.put(l.getCodProduto(),l);
    }
}
