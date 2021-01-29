package Modelo;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Classe abstrata que contém a implementação da estrutura dos Voluntários.
 */
public class Voluntario implements Serializable, Medicamentos {

    private String codVolu;
    private String nome;
    private Coordenadas gps;
    private double raio;
    private boolean livre;
    private boolean meds;
    private List<Integer> classificacao;
    private Map<Encomenda, LocalDateTime> registoV;

    // --------------------------- Constructor -------------------------

    /**
     * Construtor por omissão
     */
    public Voluntario (){
        this.codVolu = "";
        this.nome = "";
        this.gps = new Coordenadas(0,0);
        this.raio = 0;
        this.livre = true;
        this.meds = false;
        this.classificacao = new ArrayList<>();
        this.registoV  = new HashMap<>();
    }

    /**
     * Construtor parametrizado
     * @param codVolu               Codigo
     * @param nome                  Nome
     * @param gps                   Coordenadas
     * @param raio                  Raio
     * @param livre                 Livre ou não
     * @param meds                  Transporta ou não medicamentos
     * @param classificacao         Classificação
     * @param registoV              Registos
     */
    public Voluntario(String codVolu, String nome, Coordenadas gps, double raio, boolean livre,boolean meds, List<Integer> classificacao, Map<Encomenda,LocalDateTime> registoV) {
        this.codVolu = codVolu;
        this.nome = nome;
        this.gps = gps;
        this.raio = raio;
        this.livre = livre;
        this.meds = meds;
        setClassificacao(classificacao);
        setRegistoV((registoV));
    }

    /**
     * Construtorpor cópia
     * @param v         Voluntario
     */
    public Voluntario(Voluntario v){
        this.codVolu = v.getCodVolu();
        this.nome = v.getNome();
        this.gps = v.getGps();
        this.raio = v.getRaio();
        this.livre = v.isLivre();
        this.meds = v.isMeds();
        setClassificacao(v.getClassificacao());
        setRegistoV(v.getRegistoV());
    }

    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve o codigo de voluntário
     * @return String
     */
    public String getCodVolu() {
        return codVolu;
    }

    /**
     * Define o codigo de voluntário
     * @param codVolu               Codigo
     */
    public void setCodVolu(String codVolu) {
        this.codVolu = codVolu;
    }

    /**
     * Devolve o nome do voluntário
     * @return String
     */
    public String getNome() {
        return nome;
    }

    /**
     * Define o nome do voluntário
     * @param nome          Nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Devolve as coordenadas
     * @return Coordenadas
     */
    public Coordenadas getGps() {
        return gps;
    }

    /**
     * Define as coordenadas
     * @param gps           Coordenadas
     */
    public void setGps(Coordenadas gps) {
        this.gps.setLatitude(gps.getLatitude());
        this.gps.setLongitude(gps.getLongitude());
    }

    /**
     * Devolve o  raio de alcance do voluntário
     * @return double
     */
    public double getRaio() {
        return raio;
    }

    /**
     * Devolve se está ou não livre
     * @return boolean
     */
    public boolean isLivre() {
        return livre;
    }

    /**
     * Define se está ou não livre
     * @param livre             Livre ou não
     */
    public void setLivre(boolean livre) {
        this.livre = livre;
    }

    /**
     * Devolve se tem ou não medicamentos
     * @return boolean
     */
    public boolean isMeds() {
        return meds;
    }

    /**
     * Define se tem ou não medicamentos
     * @param meds              Transporta ou não medicamentos
     */
    public void setMeds(boolean meds) {
        this.meds = meds;
    }

    /**
     * Devolve a lista de  classificações
     * @return List<Integer>
     */
    public List<Integer> getClassificacao() {
        List<Integer> aux = new ArrayList<>();
        for(Integer i: this.classificacao)
            aux.add(i);
        return aux;
    }

    /**
     * Define a lista de  classificações
     * @param classificacao         Lista de classificações
     */
    public void setClassificacao(List<Integer> classificacao) {
        this.classificacao = new ArrayList<>();
        for(Integer i: classificacao)
            this.classificacao.add(i);
    }

    /**
     * Devolve os registo de encomendas já feitos
     * @return Map<Encomenda,LocalDateTime>
     */
    public Map<Encomenda,LocalDateTime> getRegistoV() {
        Map<Encomenda,LocalDateTime> aux = new HashMap<>();
        for(Map.Entry<Encomenda,LocalDateTime> v: this.registoV.entrySet()){
            aux.put(v.getKey(),v.getValue());
        }
        return aux;
    }

    /**
     * Define os registo de encomendas já feitos
     * @param registoV              Map de registos
     */
    public void setRegistoV(Map<Encomenda,LocalDateTime> registoV) {
        this.registoV = new HashMap<>();
        for(Map.Entry<Encomenda,LocalDateTime> e: registoV.entrySet()){
            this.registoV.put(e.getKey(),e.getValue());
        }
    }

    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve uma cópia da instância
     * @return Voluntário           this
     */
    public Voluntario clone(){
        return new Voluntario(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o          Objeto a comparar
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Voluntario)) return false;
        Voluntario that = (Voluntario) o;
        return Double.compare(that.getRaio(), getRaio()) == 0 &&
                isLivre() == that.isLivre() &&
                isMeds() == that.isMeds() &&
                Objects.equals(getCodVolu(), that.getCodVolu()) &&
                Objects.equals(getNome(), that.getNome()) &&
                Objects.equals(getGps(), that.getGps()) &&
                Objects.equals(getClassificacao(), that.getClassificacao()) &&
                Objects.equals(getRegistoV(), that.getRegistoV());
    }

    /**
     * Método hashCode do objeto
     * @return hash do objeto
     */
    @Override
    public int hashCode() {
        return Objects.hash(getCodVolu(), getNome(), getGps(), getRaio(), isLivre(), isMeds(), getClassificacao(), getRegistoV());
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Voluntario{");
        sb.append("codVolu='").append(codVolu).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", raio=").append(raio);
        sb.append(", livre=").append(livre);
        sb.append(", meds=").append(meds);
        sb.append(", classificacao=").append(classificacao);
        sb.append(", registoV=").append(registoV);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Função que define se está ou não dentro do alcance
     * @param gps           Coordenadas da localização destino
     * @return  boolean     true -> está dentro; false -> não está dentro
     */
    public boolean dentroRaio(Coordenadas gps){
        boolean flag;
        flag = gps.distance(this.gps) <= this.raio;
        return flag;
    }

    /**
     * Função que compara as distanciasentre 2 voluntários
     * @param u           Utilizador destino
     * @param v1          Voluntário a  comparar com this
     * @return int
     */
    public int compareDistance(Utilizador u,Voluntario v1){
        double res = this.gps.distance(u.getGps())-v1.getGps().distance(u.getGps());
        if(res == 0) return 0;
        else if(res > 0) return -1;
        return 1;
    }

    /**
     * Função que adicona um registo
     * @param e           Encomenda a adicionar
     * @param data        Data a adicionar
     */
    public void adicionaRegisto(Encomenda e,LocalDateTime data){
        this.registoV.put(e,data);
    }

    /**
     * Função que adiciona uma classificacao
     * @param e            Classificação a adicionar
     */
    public void adicionaClassi(int e){
        this.classificacao.add(e);
    }

    /**
     * Função que calcula a média das classificações
     * @return              Média das classificações
     */
    public double mediaClassificacao(){
        double media = 0;
        for(Integer d: this.classificacao)
            media += d;
        return media/this.classificacao.size();
    }

    /**
     * Função que  devolve os registos das encomendas  ordenados do mais recente para  o maisantigo
     * @param u                        Utilizador que interessa
     * @return List<Encomenda>         Lista ordenada de encomendas
     */
    public List<Encomenda> ordenaRegistVoluntario(Utilizador u){
        return this.registoV.entrySet().stream().filter(a->a.getKey().getCodUtilizador().equals(u.getCodUti()))
                                       .sorted((a,b)->(compareDatas(a.getValue(),b.getValue()))).map(Map.Entry::getKey)
                                       .collect(Collectors.toList());
    }

    /**
     * Função que compara as datas
     * @param l1          Data 1
     * @param l2          Data 2
     * @return int
     */
    public int compareDatas(LocalDateTime l1, LocalDateTime l2) {
        if (l1.isAfter(l2)) return -1;
        else if (!l1.isAfter(l2)) return 1;
        else return 0;
    }

    /**
     * Função que devolve um voluntário
     * @param u           Utilizador
     * @return Voluntário
     */
    public Voluntario transportUtili(Utilizador u) {
        for (Map.Entry<Encomenda, LocalDateTime> e : this.registoV.entrySet()) {
            if (e.getKey().getCodUtilizador().equals(u.getCodUti()))
                return this;
        }
        return null;
    }

    /**
     * Devolve se tem ou não certificado de transporte de medicamentos
     * @return boolean
     */
    public boolean aceitoTransporteMedicamentos() {
        return this.meds;
    }

    /**
     * Define se tem ou não certificado de transporte de medicamentos
     * @param state             Estado
     */
    public void aceitaMedicamentos(boolean state) {
        this.meds = state;
    }
}


