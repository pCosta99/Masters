import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public abstract class Transporta extends AppUser implements Comparable<Transporta>, Serializable {
    private String codigo;
    private String nome;
    private Coordenadas GPS;
    private double km_percorridos;
    private double raio;
    private boolean disponibilidade;
    private boolean transportaMed;
    private Map<Encomenda, TempoCustoEncomenda> encomendasRealizadas;
    private List<Integer> classificacoes;

    //////////////////////////////////// CONSTRUTORES ////////////////////////////////////

    /**
     * Construtor parametrizado da classe 'Transporta'.
     */
    public Transporta(String username, String password, String codigo, String nome, Coordenadas GPS, double raio) {
        super(username, password);
        this.codigo = codigo;
        this.nome = nome;
        this.setCoordenadas(GPS);
        this.raio = raio;
        this.km_percorridos = 0;
        this.disponibilidade = true;
        this.transportaMed = ThreadLocalRandom.current().nextBoolean();;
        this.encomendasRealizadas = new TreeMap<>();
        this.classificacoes= new ArrayList<>();
    }

    /**implements Serializable
     * Construtor por cópia da classe 'Transporta'.
     */
    public Transporta(Transporta t) {
        super(t);
        this.codigo = t.getCodigo();
        this.nome = t.getNome();
        this.GPS = t.getCoordenadas();
        this.raio = t.getRaio();
        this.disponibilidade = t.estaDisponivel();
        this.transportaMed = t.aceitoTransporteMedicamentos();
        this.encomendasRealizadas = t.getEncomendasRealizadas();
        this.classificacoes= t.getClassificacoes();
    }

    //////////////////////////////////// GETTERS E SETTERS ////////////////////////////////////


    public String getCodigo() {
        return codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public Coordenadas getCoordenadas() {
        return this.GPS.clone();
    }

    public double getKm_percorridos() {
        return this.km_percorridos;
    }

    public void setCoordenadas(Coordenadas GPS) {
        this.GPS = GPS.clone();
    }


    public double getRaio() {
        return raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public boolean estaDisponivel() {
        return disponibilidade;
    }

    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    public boolean aceitoTransporteMedicamentos() {
        return transportaMed;
    }

    public void setTransportaMed(boolean transportaMed) {
        this.transportaMed = transportaMed;
    }



    public int numeroEncomendas () {
        return this.encomendasRealizadas.values().size();
    }

    public Map<Encomenda, TempoCustoEncomenda> getEncomendasRealizadas() {
        Map<Encomenda, TempoCustoEncomenda> res = new TreeMap<>();
        this.encomendasRealizadas.entrySet().stream().forEach(e -> res.put(e.getKey().clone(), e.getValue().clone()));
        return res;
    }

    public void setEncomendasRealizadas(Map<Encomenda, TempoCustoEncomenda> encomendasRealizadas) {
        this.encomendasRealizadas = new TreeMap<>();
        encomendasRealizadas.entrySet().stream().
                             forEach(e -> this.encomendasRealizadas.put(e.getKey().clone(), e.getValue().clone()));
    }

    public List<Integer> getClassificacoes(){
        return this.classificacoes.stream().collect(Collectors.toList());
    }

    public void setClassificacoes(List<Integer> classificacoes){
        this.classificacoes = classificacoes.stream().collect(Collectors.toList());
    }

    public int getNrClassificacoes(){ return this.classificacoes.size(); }

    public int getTotalEncomendasRealizadas(){
        return this.encomendasRealizadas.size();
    }

    //////////////////////////////////// OUTROS MÉTODOS ////////////////////////////////////



    /**
     * Verifica se a Loja da encomenda esta dentro do raio de alcance
     *
     * @param l(Loja) da encomenda
     * @return boolean
     */
    private boolean lojaEstaNoRaio(Loja l){
        Coordenadas c = l.getCoordenadas();
        
        double res = this.GPS.distancia(c);
        
        return (res<=this.raio);
    }
    
    /**
     * Verifica se o utilizador da encomenda esta dentro do raio de alcance
     * 
     * @param u(Utilizador) de encomenda
     * @return boolean
     */
    private boolean utilizadorEstaNoRaio(Utilizador u){
        Coordenadas c = u.getCoordenadas();
        
        double res = this.GPS.distancia(c);
        
        return (res<=this.raio);
    }

    /**
     * Verifica se a entidade transportadora aceita a Encomenda.
     * A entidade transportadora aceita uma encomenda caso esteja disponível para fazer a entrega e caso 
     * a encomenda se encontre dentro da sua "zona" de alcance.
     * 
     * @param enc(Encomenda) a ser entregue
     * @return boolean
     */
    public boolean aceitaEncomenda(Encomenda enc){
        Loja l = super.getSistema().getLoja(enc.getCodLoja());
        Utilizador u = super.getSistema().getUtilizador(enc.getCodUtilizador());
        
        return (lojaEstaNoRaio(l) && utilizadorEstaNoRaio(u) && this.disponibilidade);
    }

    public void classifica(int classificacao){
        this.classificacoes.add((Integer) classificacao);
    }

    public int getClassificacaoMedia(){
        int total= this.classificacoes.size();
        return (int) ((this.classificacoes.stream().reduce(0, (ac,x)->ac+x))/total);
    }
    
    

    public boolean equals(Object o){
        if (o==this) return true;
        if (o==null || (o.getClass().equals(this.getClass()) == false)) return false;
        Transporta t = (Transporta) o;
        return t.getCodigo().equals(this.codigo) && t.getNome().equals(this.nome) &&
                t.getCoordenadas().equals(this.GPS) &&
                t.getRaio() == this.raio; ///<---------- é preciso ter o resto igual?
    }

    public int compareTo (Utilizador u) {
        if (this.encomendasRealizadas.size() == u.numeroEncomendas()) {
            if (this.codigo.compareTo(u.getCodigo()) < 0) return -1;
            if (this.codigo.compareTo(u.getCodigo()) > 0) return 1;
            else return 0;
        }
        if (this.encomendasRealizadas.size() < u.numeroEncomendas()) return -1;
        return 1;
    }

    public abstract void addEncomenda(Encomenda enc);

    public abstract void trataEncomendas();

    public void atualizaHistorico(Encomenda e, TempoCustoEncomenda tce) {
        this.encomendasRealizadas.put(e.clone(), tce.clone());
        this.km_percorridos += this.GPS.distancia(AppUser.getSistema().getUtilizador(e.getCodUtilizador()).getCoordenadas());
    }

    public int compareTo(Transporta t) {
        return this.codigo.compareTo(t.getCodigo());
    }
}