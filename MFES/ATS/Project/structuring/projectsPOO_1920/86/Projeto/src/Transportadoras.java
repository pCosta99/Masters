import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que represente uma empresa transportadora, contém os dados correspondentes à empresa,
 * um historico Map<String,Encomenda> cuja Key é o codigo da encomenda e o Value é a encomenda associada,
 * uma Lista com as classificações e uma variavel que dita se esta transportadora faz ou não encomendas médicas.
 */
public class Transportadoras extends Entidade implements Transportes, Serializable {
    private int nif;
    private double raio;
    private double precoKm;
    private double precoPeso;
    private boolean free;
    private Encomenda enc;
    private Map<String,Encomenda> historico;
    private List<Double> classificacao;
    private double nKms;
    private boolean transporteMedico;

    public Transportadoras() {
        super();
        this.raio = 0;
        this.precoKm = 0;
        this.precoPeso = 0;
        this.free = true;
        this.enc = null;
        this.historico = new HashMap<>();
        this.classificacao = new ArrayList<>();
        this.nKms = 0.0;
        this.transporteMedico = false;
    }

    public Transportadoras(String codEmpresa, String nomeEmpresa, GPS gps,int nif, double raio,
                           double precoKm, double precoPeso) {
        super(codEmpresa,nomeEmpresa,gps);
        this.nif = nif;
        this.raio = raio;
        this.precoKm = precoKm;
        this.precoPeso = precoPeso;
        this.free = true;
        this.enc = null;
        this.historico = new HashMap<>();
        this.classificacao = new ArrayList<>();
        this.nKms = 0.0;
        this.transporteMedico = false;
    }

    public Transportadoras(String codEmpresa, String nomeEmpresa, GPS gps,int nif, double raio,
                      double precoKm, double precoPeso, boolean free, Encomenda enc,Map<String,Encomenda> historico,List<Double> classificacao, Double nKms, boolean transporteMedico) {
        super(codEmpresa,nomeEmpresa,gps);
        this.nif = nif;
        this.raio = raio;
        this.precoKm = precoKm;
        this.precoPeso = precoPeso;
        this.free = free;
        this.enc = enc.clone();
        this.setHistorico(historico);
        this.setClassificacao(classificacao);
        this.nKms = nKms;
        this.transporteMedico = transporteMedico;
    }

    public Transportadoras(Transportadoras o){
        super(o.getCodigo(),o.getNome(),o.getCoordenadas());
        this.nif = o.getNif();
        this.raio = o.getRaio();
        this.precoKm = o.getPrecoKm();
        this.precoPeso = o.getPrecoPeso();
        this.free = o.isFree();
        this.enc = o.getEnc();
        this.setHistorico(o.getHistorico());
        this.setClassificacao(o.getClassificacao());
        this.nKms = o.getnKms();
        this.transporteMedico = o.isTransporteMedico();
    }


    public int getNif() {
        return nif;
    }

    public double getRaio() {
        return raio;
    }

    public double getPrecoKm() {
        return precoKm;
    }

    public double getPrecoPeso() {
        return precoPeso;
    }

    public boolean isFree() {
        return free;
    }

    public Encomenda getEnc() {
        if(this.enc != null)
            return enc.clone();
        return null;
    }

    public Map<String, Encomenda> getHistorico() {
        Map<String,Encomenda> aux = new HashMap<>();
        for(Map.Entry<String,Encomenda> e:this.historico.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    public List<Double> getClassificacao() {
        List<Double> aux = new ArrayList<>();
        for(Double i : this.classificacao){
            aux.add(i);
        }
        return aux;
    }

    public double getnKms() { return nKms; }

    public boolean isTransporteMedico() {
        return transporteMedico;
    }

    public void setTransporteMedico(boolean transporteMedico) {
        this.transporteMedico = transporteMedico;
    }

    public void setNif(int nif) {
        this.nif = nif;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }

    public void setPrecoPeso(double precoPeso) {
        this.precoPeso = precoPeso;
    }

    public void setFree(boolean free) {
        this.free = free;
    }

    public void setEnc(Encomenda enc) {
        this.enc = enc.clone();
    }

    public void setHistorico(Map<String,Encomenda> enc) {
        this.historico = new HashMap<>();
        for(Map.Entry<String,Encomenda> e:enc.entrySet())
            this.historico.put(e.getKey(),e.getValue().clone());
    }

    public void setClassificacao(List<Double> classificacao) {
        this.classificacao = new ArrayList<>();
        for(Double i : classificacao){
            this.classificacao.add(i);
        }
    }

    public void setnKms(double nKms) { this.nKms = nKms; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Transportadoras that = (Transportadoras) o;
        return nif == that.nif &&
                Double.compare(that.raio, raio) == 0 &&
                Double.compare(that.precoKm, precoKm) == 0 &&
                Double.compare(that.precoPeso, precoPeso) == 0 &&
                free == that.free &&
                Double.compare(that.nKms, nKms) == 0 &&
                Objects.equals(enc, that.enc) &&
                Objects.equals(historico, that.historico) &&
                Objects.equals(classificacao, that.classificacao);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Transportadoras{");
        sb.append("nif=").append(nif);
        sb.append(", raio=").append(raio);
        sb.append(", precoKm=").append(precoKm);
        sb.append(", precoPeso=").append(precoPeso);
        sb.append(", free=").append(free);
        sb.append(", enc=").append(enc);
        sb.append(", historico=").append(historico);
        sb.append(", classificacao=").append(classificacao);
        sb.append(", nKms=").append(nKms);
        sb.append('}');
        return sb.toString();
    }

    public Transportadoras clone(){
        return new Transportadoras(this);
    }

    /**
     * Método que determina a distancia a que uma tranportadora se encontra de uma identidade dada.
     * @param e entidade para a qual irá ser calculada a distancia.
     * @return retorna o valor da distância em double.
     */
    public double distancia(Entidade e){
        return this.getCoordenadas().distancia(e.getCoordenadas());
    }

    /**
     * Método que faz a entrega de uma encomenda.
     * @param u utilizador para o qual a encomenda irá ser entregue.
     * @param codCli código do cliente para o qual será entregue a encomenda.
     * @param l loja na qual irá ser requisitada a encomenda.
     */
    public void entregaEncomenda(Utilizador u, String codCli,Loja l){
        this.free = true;
        enc.requesitaEncomenda(this.getCoordenadas(),l,u.getCoordenadas());
        u.addToHistorico(this.enc.clone());
        this.historico.put(this.enc.getCodEncomenda(),this.enc.clone());
        l.addToHistorico(this.enc.clone());
        this.enc = null;
    }

    /**
     * Método que calcula o preco a ser pago pelo cliente, por distancia(transportador/loja e loja/utilizador) e por peso da encomenda.
     * @param enc Encomenda para a qual iremos calcular o preco, através do peso.
     * @param ut Utilizador para o qual iremos calcular o preco, através da distancia.
     * @param loja Loja para a qual iremos calcular o preco, através da distancia.
     * @return retorna o valor total do preco, em double.
     */
    public double calculaPreco(Encomenda enc,Entidade ut,Entidade loja){
        return (this.distancia(loja)*precoKm +loja.getCoordenadas().distancia(ut.getCoordenadas())*precoKm) + enc.getPeso()*precoPeso;
    }

    /**
     * Método que adiciona uma dada encomenda ao historico da transportadora.
     * @param e encomenda que irá ser adicionada ao historico.
     */
    public void addToHistorico(Encomenda e){
        this.historico.put(e.getCodEncomenda(),e.clone());
    }

    /**
     * Método que adiciona uma dada classificação à lista de classificações da transportadora.
     * @param i classificação a adicionar à lista.
     */
    public void addToClassificacao(double i){ this.classificacao.add(i); }

    /**
     * Método que calcula a média das classificações para a transportadora atual.
     * @return retorna a medía de classificações.
     */
    public double mediaClassificacao(){
        double res = 0;
        for(Double i : this.classificacao){
            res+=i;
        }
        return (res/this.classificacao.size());
    }

    /**
     * Método que adiciona uma dada distância, ao número de kilometros feitos pela transportadora.
     * @param dist distância a ser adicionada.
     * @return retorna o número total de kilometros após a soma.
     */
    public double addToNKms(double dist){
        return this.nKms+=dist;
    }

    /**
     * Método que calcula o preço por distância e peso.
     * @param dist distância para a qual iremos calcular o preço.
     * @param peso peso para o qual iremos calcular o preço.
     * @return retorna o preço total.
     */
    public double calculaPreco(double dist,double peso) {
        return this.precoKm*dist+this.precoPeso*peso;
    }

    /**
     * Método que determina a lista de encomendas presentes no histórico, entre duas datas.
     * @param date data inicial para filtrar encomendas.
     * @param date2 data final para filtrar encomendas.
     * @return retorna a lista das encomendas entre as datas dadas, presentes no histórico.
     */
    public List<Encomenda> historicoData(LocalDateTime date, LocalDateTime date2){
        return this.historico.values().stream().filter(a->a.getEntrega().isAfter(date) && a.getEntrega().isBefore(date2)).sorted(Comparator.comparing(Encomenda::getEntrega)).collect(Collectors.toList());
    }

    /**
     * Método que determina o total faturado pela transportadora entre duas datas.
     * @param date data inicial para filtrar encomendas.
     * @param date2 data final para filtrar encomendas.
     * @return retorna o valor total faturado pela transportadora entre as datas dadas.
     */
    public double totalFaturado(LocalDateTime date, LocalDateTime date2){
        return this.historico.values().stream()
                                      .filter(a->a.getEntrega().isAfter(date) && a.getEntrega().isBefore(date2))
                                      .map(a->a.getPeso()*this.getPrecoPeso() + this.nKms*this.precoKm)
                                      .mapToDouble(Double::doubleValue).sum();
    }
}
