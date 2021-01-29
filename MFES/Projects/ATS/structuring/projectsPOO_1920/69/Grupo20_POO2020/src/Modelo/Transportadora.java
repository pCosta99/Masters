package Modelo;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe abstrata que contém a implementação da estrutura da Transportadora.
 */
public abstract class Transportadora implements Serializable,Medicamentos {

    private String codEmpr;
    private String nomeEmpr;
    private int nif;
    private boolean multi;
    private boolean meds;
    private Coordenadas gps;
    private double raio;
    private double preco_km;
    private double preco_peso;
    private double velocidade;
    private double nKmsfeitos;
    private List<Integer> classificacao;
    private Map<Encomenda, LocalDateTime> registoT;


    // --------------------------- Constructor -------------------------

    /**
     * Construtor por omissão
     */
    public Transportadora() {
        this.codEmpr = "";
        this.nomeEmpr = "";
        this.nif = 0;
        this.multi = false;
        this.meds = false;
        this.gps = new Coordenadas();
        this.raio = 0;
        this.preco_peso = 0;
        this.preco_km = 0;
        this.velocidade = 0;
        this.nKmsfeitos = 0;
        this.classificacao = new ArrayList<>();
        this.registoT = new HashMap<>();
    }

    /**
     * Construtor parametrizado
     * @param codEmpr               Codigo
     * @param nomeEmpr              Nome
     * @param nif                   Nif
     * @param multi                 Multi ou não
     * @param meds                  Transporta ou não medicamentos
     * @param gps                   Coordenadas
     * @param raio                  Raio
     * @param preco_km              Preço por km
     * @param preco_peso            Preço por peso
     * @param velocidade            Velocidade
     * @param nKmsfeitos            Nkms feitos
     * @param classificacao         Classificação
     * @param registoT              Registos
     */
    public Transportadora(String codEmpr, String nomeEmpr, int nif, boolean multi, boolean meds, Coordenadas gps,
                          double raio, double preco_km, double preco_peso,
                          double velocidade,double nKmsfeitos,List<Integer> classificacao, Map<Encomenda, LocalDateTime> registoT) {
        this.codEmpr = codEmpr;
        this.nomeEmpr = nomeEmpr;
        this.nif = nif;
        this.multi = multi;
        this.meds = meds;
        this.gps = gps;
        this.raio = raio;
        this.preco_km = preco_km;
        this.preco_peso = preco_peso;
        this.velocidade = velocidade;
        this.nKmsfeitos = nKmsfeitos;
        setClassificacao(classificacao);
        setRegistoT(registoT);
    }

    /**
     * Construtor por cópia
     * @param t         Transportadora
     */
    public Transportadora(Transportadora t) {
        this.codEmpr = t.getCodEmpr();
        this.nomeEmpr = t.getNomeEmpr();
        this.nif = t.getNif();
        this.multi = t.isMulti();
        this.meds = t.isMeds();
        this.gps = t.getGps();
        this.raio = t.getRaio();
        this.preco_km = t.getPreco_km();
        this.preco_peso = t.getPreco_peso();
        this.velocidade = t.getVelocidade();
        this.nKmsfeitos = t.getnKmsfeitos();
        setClassificacao(t.getClassificacao());
        setRegistoT(t.getRegistoT());
    }

    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve o codigo da transportadora
     * @return String
     */
    public String getCodEmpr() {
        return codEmpr;
    }

    /**
     * Define o codigo da transportadora
     * @param codEmpr
     */
    public void setCodEmpr(String codEmpr) {
        this.codEmpr = codEmpr;
    }

    /**
     * Devolve o nome da trasnportadora
     * @return String
     */
    public String getNomeEmpr() {
        return nomeEmpr;
    }

    /**
     * Define o nome da trasnportadora
     * @param nomeEmpr          Nome
     */
    public void setNomeEmpr(String nomeEmpr) {
        this.nomeEmpr = nomeEmpr;
    }

    /**
     * Devolve o nif
     * @return int
     */
    public int getNif() {
        return nif;
    }

    /**
     * Define o nif
     * @param nif           Nif
     */
    public void setNif(int nif) {
        this.nif = nif;
    }

    /**
     * Devolve se é ou não uma transportadora multi
     * @return              Multi ou não
     */
    public boolean isMulti() {
        return multi;
    }

    /**
     * Devolve se transporta ou não medicamentos
     * @return              Se transporta ou não medicamentos
     */
    public boolean isMeds() {
        return meds;
    }

    /**
     * Devolve as Coordenadas da transportadora
     * @return Coordenadas
     */
    public Coordenadas getGps() {
        return gps;
    }

    /**
     * Define as Coordenadas da transportadora
     * @param gps               Coordenadas
     */
    public void setGps(Coordenadas gps) {
        this.gps.setLatitude(gps.getLatitude());
        this.gps.setLongitude(gps.getLongitude());
    }

    /**
     * Devolve o raio de alcance da transportadora
     * @return double
     */
    public double getRaio() {
        return raio;
    }

    /**
     * Define o raio de alcance da transportadora
     * @param raio              Raio
     */
    public void setRaio(double raio) {
        this.raio = raio;
    }

    /**
     * Devolve o preço por km
     * @return double
     */
    public double getPreco_km() {
        return preco_km;
    }

    /**
     * Define o preço por km
     * @param preco_km              Preço por km
     */
    public void setPreco_km(double preco_km) {
        this.preco_km = preco_km;
    }

    /**
     * Devolve o preço por peso
     * @return double
     */
    public double getPreco_peso() {
        return preco_peso;
    }

    /**
     * Define o preço por peso
     * @param preco_peso            Preço por peso
     */
    public void setPreco_peso(double preco_peso) {
        this.preco_peso = preco_peso;
    }

    /**
     * Devolve a velocidade media
     * @return double
     */
    public double getVelocidade() {
        return velocidade;
    }

    /**
     * Define a velocidade media
     * @param velocidade            Velocidade
     */
    public void setVelocidade(double velocidade) {
        this.velocidade = velocidade;
    }

    /**
     * Devolve o numero de kms já feitos
     * @return double
     */
    public double getnKmsfeitos() {
        return nKmsfeitos;
    }

    /**
     * Devolve a lista de classificações feitas
     * @return List<Integer>
     */
    public List<Integer> getClassificacao() {
        List<Integer> aux = new ArrayList<>();
        for(Integer i: this.classificacao)
            aux.add(i);
        return aux;
    }

    /**
     * Define a classificação
     * @param classificacao         Lista de inteiros
     */
    public void setClassificacao(List<Integer> classificacao) {
        this.classificacao = new ArrayList<>();
        for(Integer i: classificacao)
            this.classificacao.add(i);
    }

    /**
     * Devolve as encomendas e o tempo já feitas pela transportadora
     * @return Map<Encomenda,LocalDateTime>
     */
    public Map<Encomenda, LocalDateTime> getRegistoT() {
        Map<Encomenda, LocalDateTime> aux = new HashMap<>();
        for (Map.Entry<Encomenda, LocalDateTime> e : this.registoT.entrySet()) {
            aux.put(e.getKey(), e.getValue());
        }
        return aux;
    }

    /**
     * Define as encomendas e o tempo já feitas pela transportadora
     * @param registoT              Map de registos
     */
    public void setRegistoT(Map<Encomenda, LocalDateTime> registoT) {
        this.registoT = new HashMap<>();
        for (Map.Entry<Encomenda, LocalDateTime> e : registoT.entrySet()) {
            this.registoT.put(e.getKey(), e.getValue());
        }
    }

    // --------------------------- Auxiliaries -------------------------

    /**
     * Devolve uma cópia da instância
     * @return Transpoortadora
     */
    public abstract Transportadora clone();

    /**
     * Função que calcula o custo da Encomenda
     * @param e                      Encomenda
     * @param u                      Utilizador
     * @param loja                   Loja
     * @return double                Custo da encomenda
     */
    public abstract double custoEncomenda(Encomenda e, Utilizador u, Coordenadas loja);

    /**
     * Função que calcula o tempo da Encomenda
     * @param e                      Encomenda
     * @param u                      Utilizador
     * @param loja                   Loja
     * @return double                Tempo da  encomenda
     */
    public abstract double tempodeEncomenda(Encomenda e, Utilizador u, Loja loja);

    /**
     * Adiciona o Encomenda aos registos
     * @param e                Encomenda a adicionar
     * @param data             Data e hora da encomenda
     */
    public void adicionaRegisto(Encomenda e, LocalDateTime data) {
        this.registoT.put(e, data);
    }

    /**
     * Função que informa se utilizador está entre raio ou não
     * @param utilizador                Coordenadas do utilizador
     * @return boolean                  Dentro ou não do raio
     */
    public abstract boolean dentroRaio(Coordenadas utilizador);


    /**
     * Verifica a igualdade com outro objeto
     * @param o          Objeto a comparar
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Transportadora)) return false;
        Transportadora that = (Transportadora) o;
        return getNif() == that.getNif() &&
                isMulti() == that.isMulti() &&
                isMeds() == that.isMeds() &&
                Double.compare(that.getRaio(), getRaio()) == 0 &&
                Double.compare(that.getPreco_km(), getPreco_km()) == 0 &&
                Double.compare(that.getPreco_peso(), getPreco_peso()) == 0 &&
                Double.compare(that.getVelocidade(), getVelocidade()) == 0 &&
                Double.compare(that.getnKmsfeitos(), getnKmsfeitos()) == 0 &&
                Objects.equals(getCodEmpr(), that.getCodEmpr()) &&
                Objects.equals(getNomeEmpr(), that.getNomeEmpr()) &&
                Objects.equals(getGps(), that.getGps()) &&
                Objects.equals(getClassificacao(), that.getClassificacao()) &&
                Objects.equals(getRegistoT(), that.getRegistoT());
    }

    /**
     * Método hashCode do objeto
     * @return hash do objeto
     */
    @Override
    public int hashCode() {
        return Objects.hash(getCodEmpr(), getNomeEmpr(), getNif(), isMulti(), isMeds(), getGps(), getRaio(), getPreco_km(), getPreco_peso(), getVelocidade(), getnKmsfeitos(), getClassificacao(), getRegistoT());
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Modelo.Transportadora{");
        sb.append("codEmpr='").append(codEmpr).append('\'');
        sb.append(", nomeEmpr='").append(nomeEmpr).append('\'');
        sb.append(", nif=").append(nif);
        sb.append(", multi=").append(multi);
        sb.append(", meds=").append(meds);
        sb.append(", gps=").append(gps);
        sb.append(", raio=").append(raio);
        sb.append(", preco_km=").append(preco_km);
        sb.append(", preco_peso=").append(preco_peso);
        sb.append(", velocidade=").append(velocidade);
        sb.append(", nKmsfeitos=").append(nKmsfeitos);
        sb.append(", classificacao=").append(classificacao);
        sb.append(", registoT=").append(registoT);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Funcção que ordena os registo das encomendas de um dado utilizador pela mais recente  à mais antiga
     * @param u                      Utilizador
     * @return List<Encomenda>       Lista ordenada
     */
    public List<Encomenda> ordenaRegisto(Utilizador u) {
        return this.registoT.entrySet().stream().filter(a -> a.getKey().getCodUtilizador().equals(u.getCodUti()))
                .sorted((a, b) -> compareDatas(a.getValue(), b.getValue())).map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    /**
     * Comparador de datas
     * @param l1     Data 1
     * @param l2     Datd 2
     * @return int   Valor da comparação
     */
    public int compareDatas(LocalDateTime l1, LocalDateTime l2) {
        if (l1.isAfter(l2)) return -1;
        else if (!l1.isAfter(l2)) return 1;
        else return 0;
    }


    /**
     * Função que nos indica a transportadora de um utilizador
     * @param u                        Utilizador
     * @return Transportadora
     */
    public Transportadora transportUtili(Utilizador u) {
        for (Map.Entry<Encomenda, LocalDateTime> e : this.registoT.entrySet()) {
            if (e.getKey().getCodUtilizador().equals(u.getCodUti()))
                return this;
        }
        return null;
    }


    /**
     * Função que adiciona uma nova classificação à lista
     * @param e                        Classificação
     */
    public void adicionaClassi(int e){
        this.classificacao.add(e);
    }

    /**
     * Função que nos dá a média de classificações
     * @return double                  Média
     */
    public double mediaClassificacao(){
        double media = 0;
        for(Integer d: this.classificacao)
            media += d;
        return media/this.classificacao.size();
    }


    /**
     * Função que nos dá a faturação da transportadora, dado um tempo incial e um tempo final
     * @param i                        Tempo inicial
     * @param f                        Tempo final
     * @param p                        Classe TrazAquiModel
     * @return double                  Faturação
     */
    public double faturacaoDadoTempo(LocalDateTime i, LocalDateTime f, TrazAquiModel p){
        double fat = 0;
        List<Encomenda> enc = this.registoT.entrySet().stream().filter(a->a.getValue().isAfter(i) && a.getValue().isBefore(f))
                                  .map(Map.Entry::getKey).collect(Collectors.toList());
        for(Encomenda e: enc){
            Utilizador u = p.getUtilizador().get(e.getCodUtilizador());
            Loja l = p.getLoja().get(e.getCodLoja());
            fat += custoEncomenda(e,u,l.getGps());
        }
        return fat;
    }


    /**
     * Função que incrementa o numero de kms percorridos pela transportadora
     * @param u                         Coordenadas
     */
    public void incrementa(Coordenadas u){
        this.nKmsfeitos += this.gps.distance(u);
    }


    /**
     * Função que compara kms percorridos por uma transportadora
     * @param t                         Transportadora
     * @return int
     */
    public int compareKm(Transportadora t){
        if(this.nKmsfeitos  ==  t.getnKmsfeitos()) return 0;
        else if(this.nKmsfeitos > t.getnKmsfeitos()) return -1;
        return 1;
    }


    /**
     * Função que aceita medicamentos ou não
     * @param state                 Estado
     */
    @Override
    public void aceitaMedicamentos(boolean state) {
        this.meds = state;
    }


    /**
     * Função que nos diz se uma transportadora aceita fazer transporte de medicamentos
     * @return boolean
     */
    @Override
    public boolean aceitoTransporteMedicamentos() {
        return this.meds;
    }
}