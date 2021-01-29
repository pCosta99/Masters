package Model;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Encomenda implements java.io.Serializable{

    private String codEnc;
    private String codLoja;
    private String codUtilizador;
    private LocalDateTime data;
    private double peso;
    private double preco;
    private String estado;
    private boolean medica;
    private String codTransportadora;
    private int classificacao;
    private Map<String,Duration> empresasAguardar;
    private List<LinhaDeEncomenda> linhaEnc;
    private Duration tempoDeEntrega;

    public Encomenda(){
        this.codEnc = "";
        this.codLoja = "";
        this.codUtilizador = "";
        this.data = LocalDateTime.now();
        this.peso = 0;
        this.preco = 0;
        this.estado = "Pendente";
        this.medica = false;
        this.codTransportadora = "";
        this.classificacao = -1;
        this.empresasAguardar = new HashMap<>();
        this.linhaEnc = new ArrayList<>();
        this.tempoDeEntrega = Duration.ZERO;
    }

    /**
     * Construtor parametrizado
     * @param codEnc
     * @param codLoja
     * @param codUtilizador
     * @param data
     * @param peso
     * @param preco
     * @param estado
     * @param tipo
     * @param linhaEnc
     */
    public Encomenda (String codEnc, String codLoja, String codUtilizador, LocalDateTime data, double peso, double preco,
                      String estado, boolean medica, String tipo, String codTransp, int classificacao,
                      Map<String,Duration> empresas,  List<LinhaDeEncomenda> linhaEnc){
        this.codEnc = codEnc;
        this.codLoja = codLoja;
        this.codUtilizador = codUtilizador;
        this.data = data;
        this.peso = peso;
        this.preco = preco;
        this.estado = estado;
        this.medica = medica;
        this.codTransportadora = codTransp;
        this.classificacao = classificacao;
        this.empresasAguardar = new HashMap<>();

        for(Map.Entry<String,Duration> s : empresas.entrySet()){
            this.empresasAguardar.put(s.getKey(),s.getValue());
        }


        this.linhaEnc = new ArrayList<LinhaDeEncomenda>();

        for (LinhaDeEncomenda e : linhaEnc){
            this.linhaEnc.add(e.clone());
        }
        this.tempoDeEntrega = Duration.ZERO;
    }

    /**
     * Construtor parametrizado
     * @param codEnc
     * @param codLoja
     * @param codUtilizador
     * @param data
     * @param peso
     * @param linhaEnc
     */
    public Encomenda (String codEnc, String codLoja, String codUtilizador, LocalDateTime data, double peso, List<LinhaDeEncomenda> linhaEnc){
        this.codEnc = codEnc;
        this.codLoja = codLoja;
        this.codUtilizador = codUtilizador;
        this.codTransportadora = "";
        this.data = data;
        this.peso = peso;
        //Calcular o preço
        this.preco = 0;
        this.estado = "Pendente";
        this.medica = false;
        this.classificacao = -1;
        this.empresasAguardar = new HashMap<>();
        this.linhaEnc = new ArrayList<LinhaDeEncomenda>();

        for (LinhaDeEncomenda e : linhaEnc){
            this.linhaEnc.add(e.clone());
        }
        this.tempoDeEntrega = Duration.ZERO;
    }

    /**
     * Construtor por cópia
     * @param encomenda
     */
    public Encomenda (Encomenda encomenda){
        setCodEnc(encomenda.getCodEnc());
        setCodLoja(encomenda.getCodLoja());
        setCodUtilizador(encomenda.getCodUtilizador());
        setData(encomenda.getData());
        setPeso(encomenda.getPeso());
        setPreco(encomenda.getPreco());
        setEstado(encomenda.getEstado());
        setMedica(encomenda.getMedica());
        setMedica(encomenda.getMedica());
        setCodTransportadora(encomenda.getCodTransportadora());
        setClassificacao(encomenda.getClassificacao());
        setEmpresasAguardar(encomenda.getEmpresasAguardar());
        setLinhaEnc(encomenda.getLinhaEnc());
        setTempoEntrega(encomenda.getTempoEntrega());
    }

    /**
     * Devolve o codEnc
     * @return
     */
    public String getCodEnc() {
        return this.codEnc;
    }

    /**
     * Atualiza o valor setCodEnc
     * @param codEnc
     */
    public void setCodEnc(String codEnc) {
        this.codEnc = codEnc;
    }

    /**
     * Devolve o codLoja
     * @return
     */
    public String getCodLoja() {
        return this.codLoja;
    }

    /**
     * Atualiza o valor setCodLoja
     * @param codLoja
     */
    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    /**
     * Devolve o codUtilizador
     * @return
     */
    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    /**
     * Atualiza o codUtilizador
     * @param codUtilizador
     */
    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    /**
     * Devolve o data
     * @return
     */
    public LocalDateTime getData() {
        return this.data;
    }

    /**
     * Atualiza o data
     * @param data
     */
    public void setData(LocalDateTime data) {
        this.data = data;
    }

    /**
     * Devolve o peso
     * @return
     */
    public double getPeso() {
        return this.peso;
    }

    /**
     * Atualiza o peso
     * @param peso
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Devolve o preco
     * @return
     */
    public double getPreco() {
        return this.preco;
    }

    /**
     * Atualiza o preco
     * @param preco
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Devolve o estado
     * @return
     */
    public String getEstado() {
        return this.estado;
    }

    /**
     * Atualiza o estado
     * @param estado
     */
    public void setEstado(String estado) {
        this.estado = estado;
    }

    /**
     * Devolve o tipo
     * @return
     */
    public boolean getMedica() {
        return this.medica;
    }

    /**
     * Atualiza o tipo
     * @param med
     */
    public void setMedica(boolean med) {
        this.medica = med;
    }

    /**
     * Devolve o código da transportadora
     */
    public String getCodTransportadora(){
        return this.codTransportadora;
    }

    /**
     * Atualiza o código da transportadora
     */
    public void setCodTransportadora(String novoCordTransp){
        this.codTransportadora = novoCordTransp;
    }


    /**
     * Devolve o linhaEnc
     * @return
     */
    public List<LinhaDeEncomenda> getLinhaEnc() {
        List<LinhaDeEncomenda> prods = new ArrayList<LinhaDeEncomenda>();

        for (LinhaDeEncomenda e : this.linhaEnc){
            prods.add(e.clone());
        }
        return prods;
    }

    /**
     * Atualiza o linhaEnc
     * @param linhaEnc
     */
    public void setLinhaEnc(List<LinhaDeEncomenda> linhaEnc) {
        this.linhaEnc = new ArrayList<LinhaDeEncomenda>();

        for (LinhaDeEncomenda e : linhaEnc){
            this.linhaEnc.add(e.clone());
        }
    }

    /**
     * Devolve a classificação
     * @return
     */
    public int getClassificacao(){
        return this.classificacao;
    }


    /**
     * Atualiza a classificação
     * @param novaClassificacao
     */
    public void setClassificacao(int novaClassificacao){
        this.classificacao = novaClassificacao;
    }

    /**
     * Devolve as empresas a aguardar aprovação para transportar
     * @return
     */
    public Map<String,Duration> getEmpresasAguardar(){
        Map<String,Duration> res = new HashMap<>();

        for(Map.Entry<String,Duration> s : this.empresasAguardar.entrySet())
            res.put(s.getKey(),s.getValue());

        return res;
    }

    /**
     * Atualiza as empresas a aguardar aprovação para transportar
     * @param empresas
     */
    public void setEmpresasAguardar(Map<String,Duration> empresas){
        this.empresasAguardar = new HashMap<>();

        for(Map.Entry<String,Duration> s : empresas.entrySet())
            this.empresasAguardar.put(s.getKey(),s.getValue());
    }

    /**
     * Devolve o tempo de entrega de uma encomenda
     * @return
     */
    public Duration getTempoEntrega(){
        return this.tempoDeEntrega;
    }

    /**
     * Atualiza o tempo de entrega
     * @param novoTempo
     */
    public void setTempoEntrega(Duration novoTempo){
        this.tempoDeEntrega = novoTempo;
    }

    /**
     * Adiciona um código da empresa à lista das empresas a aguardar aprovação
     * para transportar a encomenda
     * @param codEmpresa
     */
    public void adicionaEmpresaAguarda(String codEmpresa, Duration tempo){
        if(!this.empresasAguardar.containsKey(codEmpresa))
            this.empresasAguardar.put(codEmpresa,tempo);
    }

    /**
     * Remove um código da empresa à lista das empresas a aguardar aprovação
     * para transportar a encomenda
     * @param codEmpresa
     */
    public void removeEmpresaAguarda(String codEmpresa){
        if(!this.empresasAguardar.containsKey(codEmpresa))
            this.empresasAguardar.remove(codEmpresa);
    }

    /**
     * Método que devolve o número de empresas que aguardam
     * aprovação para a encomenda
     * @return
     */
    public int numEmpresasAguardar(){
        return this.empresasAguardar.size();
    }

    /**
     * Devolve uma cópia da instância
     * @return
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {

        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;

        Encomenda encomenda = (Encomenda) o;

        boolean prod = (this.linhaEnc.size() == encomenda.getLinhaEnc().size());

        List<LinhaDeEncomenda> prods = encomenda.getLinhaEnc();

        if(prod){
            for (int i = 0; i < this.linhaEnc.size(); i++){
                prod = prod && this.linhaEnc.get(i).equals(prods.get(i));
            }
        }

        return this.codEnc == encomenda.getCodEnc() &&
                this.codLoja.equals(encomenda.getCodLoja()) &&
                this.codUtilizador.equals(encomenda.getCodUtilizador()) &&
                this.data.equals(encomenda.getData()) &&
                this.peso == encomenda.getPeso() &&
                this.preco == encomenda.getPreco() &&
                this.estado.equals(encomenda.getEstado()) &&
                this.codTransportadora.equals(encomenda.getCodTransportadora()) &&
                this.classificacao == encomenda.getClassificacao() &&
                this.empresasAguardar.equals(encomenda.getEmpresasAguardar()) &&
                this.medica == encomenda.getMedica() & prod &&
                this.tempoDeEntrega.equals(encomenda.getTempoEntrega());
    }

    /**
     * Devole uma representação textual de Encomenda
     * @return
     */
    @Override
    public String toString() {
        return "Encomenda{" +
                "codEnc=" + codEnc +
                ", codLoja='" + codLoja + '\'' +
                ", codUtilizador='" + codUtilizador + '\'' +
                ", data='" + data.toString() + '\'' +
                ", peso=" + peso +
                ", preco=" + preco +
                ", estado='" + estado + '\'' +
                ", medica='" + medica + '\'' +
                ", codTransportadora=" + codTransportadora + '\'' +
                ", classificação= " + classificacao + '\'' +
                ", empresas a aguardas aprovação= " + empresasAguardar + '\'' +
                ", linhaEnc= " + linhaEnc.toString() + '\'' +
                ", tempo de entrega= " + tempoDeEntrega +
                '}';
    }

    public void addLinhaEnc(LinhaDeEncomenda linhaDeEncomenda, String quantidade){
        this.linhaEnc.add(linhaDeEncomenda.clone());
        this.peso += Double.parseDouble(quantidade);
        this.preco += ((Double.parseDouble(quantidade))/linhaDeEncomenda.getPeso())*linhaDeEncomenda.getPreco();
    }

}
