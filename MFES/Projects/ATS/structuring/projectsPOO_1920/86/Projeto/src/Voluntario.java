import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que represente um voluntário, contém os dados correspondentes ao voluntário,
 * um historico Map<String,Encomenda> cuja Key é o codigo da encomenda e o Value é a encomenda associada,
 * uma Lista com as classificações e uma variavel que dita se esta transportadora faz ou não encomendas médicas.
 */
public class Voluntario extends Entidade implements Transportes, Serializable {
    private double raio;
    private boolean free;
    private Encomenda enc;
    private Map<String,Encomenda> historico;
    private List<Double> classificacao;
    private Double nKms;
    private boolean transporteMedico;

    public Voluntario() {
        super();
        this.raio = 0;
        this.free = true;
        this.enc = null;
        this.historico = new HashMap<>();
        this.classificacao = new ArrayList<>();
        this.nKms = 0.0;
        this.transporteMedico = false;
    }

    public Voluntario(String codVoluntario, String nome, GPS gps, double raio) {
        super(codVoluntario,nome,gps);
        this.raio = raio;
        this.free = true;
        this.enc = null;
        this.historico = new HashMap<>();
        this.classificacao = new ArrayList<>();
        this.nKms = 0.0;
        this.transporteMedico = false;
    }

    public Voluntario(String codVoluntario, String nome, GPS gps, double raio, boolean free, Encomenda enc, Map<String,Encomenda> historico, List<Double> classificacao, Double nKms, boolean transporteMedico) {
        super(codVoluntario,nome,gps);
        this.raio = raio;
        this.free = free;
        this.enc = enc.clone();
        this.setHistorico(historico);
        this.setClassificacao(classificacao);
        this.nKms = nKms;
        this.transporteMedico = transporteMedico;
    }


    public Voluntario(Voluntario o){
        super(o.getCodigo(),o.getNome(),o.getCoordenadas());
        this.raio = o.getRaio();
        this.free = o.isFree();
        this.enc = (o.getEnc());
        this.setHistorico(o.getHistorico());
        this.setClassificacao(o.getClassificacao());
        this.nKms = o.getnKms();
        this.transporteMedico = o.isTransporteMedico();
    }

    public double getRaio() {
        return raio;
    }

    public boolean isFree() {
        return free;
    }

    public Encomenda getEnc() {
        if(enc != null)
            return enc.clone();
        return null;
    }

    public Map<String,Encomenda> getHistorico() {
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

    public Double getnKms() { return nKms; }

    public boolean isTransporteMedico() {
        return transporteMedico;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public void setFree(boolean free) {
        this.free = free;
    }

    public void setEnc(Encomenda enc) {
        if(enc == null) this.enc = null;
        else this.enc = enc.clone();
    }

    public void setHistorico(Map<String,Encomenda> enc) {
        this.historico = new HashMap<>();
        for(Map.Entry<String,Encomenda> e:enc.entrySet())
            this.historico.put(e.getKey(),e.getValue().clone());
    }

    public void setnKms(Double nKms) { this.nKms = nKms; }

    public void setClassificacao(List<Double> classificacao) {
        this.classificacao = new ArrayList<>();
        for(Double i : classificacao){
            this.classificacao.add(i);
        }
    }

    public void setTransporteMedico(boolean transporteMedico) {
        this.transporteMedico = transporteMedico;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Voluntario that = (Voluntario) o;
        return Double.compare(that.raio, raio) == 0 &&
                free == that.free &&
                Objects.equals(enc, that.enc) &&
                Objects.equals(historico, that.historico) &&
                Objects.equals(classificacao, that.classificacao);
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("Voluntario:");
        sb.append(getCodigo()).append(",");
        sb.append(getNome()).append(",");
        sb.append(getCoordenadas().toString());
        sb.append(getRaio()).append(",");
        //sb.append(getEnc().toString());
        return sb.toString();
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }

    /**
     * Método que determina a distancia a que um voluntario se encontra de uma identidade dada.
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
     * Método que adiciona uma dada encomenda ao historico do voluntario.
     * @param e encomenda que irá ser adicionada ao historico.
     */
    public void addToHistorico(Encomenda e){
        this.historico.put(e.getCodEncomenda(),e.clone());
    }

    /**
     * Método que adiciona uma dada classificação à lista de classificações do voluntario.
     * @param i classificação a adicionar à lista.
     */
    public void addToClassificacao(double i){ this.classificacao.add(i); }

    /**
     * Método que calcula a média das classificações para o voluntario atual.
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
     * Método que adiciona uma dada distância, ao número de kilometros feitos pelo voluntario.
     * @param dist distância a ser adicionada.
     * @return retorna o número total de kilometros após a soma.
     */
    public double addToNKms(double dist){
        return this.nKms+=dist;
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

}
