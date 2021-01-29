package Users;

import Geral.GPS;
import Stock.Encomenda;
import Stock.InfoProduto;
import Stock.LinhaEncomenda;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;


public class Loja extends User implements Serializable {

    private boolean informa_sobre_loja;
    private GPS gps;
    private String nome;
    private List<Encomenda> encomendas;
    private int qtd_pessoas_fila;
    private double tempo_médio_atendimento;
    private boolean tem_encomendas;
    private Map<String,InfoProduto> produtos;

    /**
     *   Construtor por omissão
     */
    public Loja(){
        super();
        this.qtd_pessoas_fila = 0;
        this.nome = "";
        this.encomendas = new ArrayList<>();
        this.gps = new GPS();
        this.informa_sobre_loja = false;
        this.tem_encomendas = false;
        this.tempo_médio_atendimento = 0;
        this.produtos = new TreeMap<>();
    }

    /**
     * Construtor por parâmetros.
     * @param cL Código da Loja.
     * @param nL Nome da Loja.
     * @param password Password de login
     * @param l Localização GPS da Loja.
     * @param informa Diz se a Loja informa sobre si ou não.
     * @param t Tempo médio de atendimento da Loja.
     */
    public Loja(String cL, String nL, String password, GPS l,boolean informa, double t){
        super(cL,password);
        this.nome = nL;
        this.gps = l;
        this.qtd_pessoas_fila = 0;
        this.encomendas = new ArrayList<>();
        this.informa_sobre_loja = informa;
        this.tempo_médio_atendimento = t;
        this.tem_encomendas = false;
        this.produtos = new TreeMap<>();
    }

    /**
     * Construtor por clonagem.
     * @param l Loja a clonar.
     */
    public Loja(Loja l){
        super(l);
        this.nome = l.getNome();
        this.gps = l.getGPS();
        this.qtd_pessoas_fila = l.getQtd_pessoas_fila();
        this.encomendas = l.getEncomendas();
        this.informa_sobre_loja = l.isInforma_sobre_loja();
        this.tempo_médio_atendimento = l.getTempo_médio_atendimento();
        this.tem_encomendas = l.isTem_encomendas();
        this.produtos = l.getProdutos();
    }

    /**
     * Método que devolve a quantidade de pessoas na fila de espera.
     * @return Quantidade de pessoas na fila.
     */
    public int getQtd_pessoas_fila() {
        return qtd_pessoas_fila;
    }

    /**
     * Método de que devolve a lista de encomendas que uma Loja possui.
     * @return Lista de encomendas.
     */
    public List<Encomenda> getEncomendas(){
        return this.encomendas.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    /**
     * Método que averigua se uma loja informa ou não sobre si.
     * @return boolean.
     */
    public boolean isInforma_sobre_loja() {
        return informa_sobre_loja;
    }

    /**
     * Método que devolve o tempo médio de atendimento da loja.
     * @return Tempo médio de atendimento.
     */
    public double getTempo_médio_atendimento() {
        return tempo_médio_atendimento;
    }

    /**
     * Método que averigua se uma loja tem encomendas para entregar.
     * @return boolean.
     */
    public boolean isTem_encomendas() {
        return tem_encomendas;
    }


    /**
     *   setter da quantidade de pessoas na fila da Users.Loja
     */
    public void setQtd_pessoas_fila(int qtd) {
        this.qtd_pessoas_fila = qtd;
    }
    /**
     *   Setter das encomendas já realizadas pela Users.Loja
     */
    public void setEncomendas(List<Encomenda> e){
        this.encomendas = new ArrayList<>();
        for(Encomenda ec : e){
            this.encomendas.add(ec.clone());
        }
    }

    /**
     * Método que define se uma loja informa ou não sobre a sua fila de espera.
     * @param informa_sobre_loja que diz se informa ou não.
     */
    public void setInforma_sobre_loja(boolean informa_sobre_loja) {
        this.informa_sobre_loja = informa_sobre_loja;
    }

    /**
     * Método que define o tempo médio de atendimento na loja.
     * @param tempo_médio_atendimento que é o tempo médio.
     */
    public void setTempo_médio_atendimento(double tempo_médio_atendimento) {
        this.tempo_médio_atendimento = tempo_médio_atendimento;
    }

    /**
     * Método que define se uma loja tem encomendas para entregar.
     * @param tem_encomendas que diz se tem ou não.
     */
    public void setTem_encomendas(boolean tem_encomendas) {
        this.tem_encomendas = tem_encomendas;
    }


    /**
     * Método que devolve um Map com encomendas e a sua descrição.
     * @return Map com a informação.
     */
    public Map<String,String> showEncomendasPorEntregar(){
        Map<String,String> aux = new TreeMap<>();
        for(Encomenda e : this.encomendas){
            for(LinhaEncomenda l : e.getProdutos())
                aux.put(e.getCodEncomenda(),l.getDescricao());
        }
        return aux;
    }

    /**
     * Método que devolve os produtos à venda numa loja.
     * @return Informação sobre os produtos à venda.
     */
    public Map<String,InfoProduto> getProdutos(){
        Map<String,InfoProduto> aux = new TreeMap<>();
        for(Map.Entry<String,InfoProduto>  a : this.produtos.entrySet()){
            aux.put(a.getKey(),a.getValue().clone());
        }
        return aux;
    }

    /**
     * Método que define os produtos à venda numa loja.
     * @param a que contém a informação sobre os produtos.
     */
    public void setProdutos(Map<String,InfoProduto> a){
        this.produtos = new TreeMap<>();
        a.entrySet().stream().forEach(v -> this.produtos.put(v.getKey(),v.getValue().clone()));
    }

    /**
     * Método que devolve uma localização da Loja.
     * @return Localização.
     */
    public GPS getGPS() {
        return gps.clone();
    }

    /**
     * Método que define a localização da Loja
     * @param gps que é a localização.
     */
    public void setGps(GPS gps) {
        this.gps = gps;
    }

    /**
     * Método que devolve o nome da Loja.
     * @return Nome da loja.
     */
    public String getNome() {
        return nome;
    }

    /**
     * Método que define o nome da Loja.
     * @param nome que é o nome.
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método que adiciona um novo produto à lista de produtos da loja
     * @param codP que é o código do produto.
     * @param descricao que é a descrição do prduto.
     * @param valor que é o preço do produto.
     */
    public void addProdLoja(String codP, String descricao, double valor, double peso){
        this.produtos.putIfAbsent(codP,new InfoProduto(descricao,valor,peso));
    }

    /**
     * Método que devolve o código da loja.
     * @return Código da loja.
     */
    public String getCodigo(){
        return super.getCode();
    }

    /**
     * Método que define o código da loja.
     * @param c que é o código.
     */
    public void setCodigo(String c){
        super.setCode(c);
    }

    /**
     * Método que adiciona uma encomenda a lista de encomendas por entregar.
     * @param e que é a Encomenda.
     */
    public void addEncomendaOnHold(Encomenda e){
        this.encomendas.add(e);
        incrementaFilaEspera();
    }

    /**
     * Método que incrementa a quantidade de pessoas na fila de espera.
     */
    public void incrementaFilaEspera(){
        this.qtd_pessoas_fila++;
    }

    /**
     * Método que decrementa a quantidade de pessoas na fila de espera.
     */
    public void decrementaFilaEspera(){
        this.qtd_pessoas_fila--;
    }

    /**
     * Método que remove uma encomenda à espera de ser entregue.
     * @param e
     */
    public void removeEncomenda(Encomenda e){
        this.encomendas.remove(e);
        decrementaFilaEspera();
    }

    //Clone, toString, equals

    /**
     * Método que faz uma cópia de uma Loja.
     * @return Cópia de uma Loja.
     */
    public Loja clone(){
        return new Loja(this);
    }

    /**
     * Método que converte numa String a informação sobre uma Loja.
     * @return String com a informação da Loja.
     */
    public String toString(){
        return "Loja:"+super.getCode()+","+this.getNome()+","+this.getGPS().toString();
    }

    /**
     * Método que compara duas Lojas.
     * @param obj Loja a comparar.
     * @return Boolean com o resultado da comparação.
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Loja l = (Loja) obj;
        return (this.getCodigo().equals(l.getCodigo())
                && this.getNome().equals(l.getNome())
                && this.getGPS().equals(l.getGPS())
                && this.qtd_pessoas_fila == l.getQtd_pessoas_fila()
                && this.encomendas.equals(l.getEncomendas()));
    }
}