import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/*  TODO
    - clones
    - resolver ciclos
    - duas cenas que precisam de LinhaEncomenda??
 */

/**
 * Classe que contem a informaçao da transportadora
 */
public class Transportadora implements Serializable {
    /*Posição da sede*/
    private Posicao posicao;

    /*Código*/
    private String codEmpresa;

    /*Nome*/
    private String nomeEmpresa;

    /*Número de Identificação Fiscal*/
    private String nif;

    /*Raio de ação do trabalhador*/
    private double raio;

    /*Preço por kilómetro*/
    private double ppkm;

    /*Quantidade de kms percorridos*/
    private double kms;

    /*Velocidade media*/
    private double vm;

    /*Capacidade de encomendas*/
    private double capPeso;

    /*Disponibilidade*/
    private boolean availability;

    /*Disponibilidade para transportes medicos*/
    private boolean mavailability;

    /*Certeficado para transportes medicos*/
    private boolean tmedico;

    /*Registo de encomendas*/
    private Map<String, Encomenda> registo;

    /*Fila de espera de encomendas*/
    private List<Encomenda> queue;

    /*Lista de classificaçoes*/
    private List<Float> classificacoes;


    /**
     * Construtores
     */

    /**
     * Por omissão
     */
    public Transportadora() {
        this.posicao = new Posicao();
        this.codEmpresa = null;
        this.nomeEmpresa = null;
        this.nif = null;
        this.raio = 0;
        this.ppkm = 0.0;
        this.kms = 0;
        this.vm = 0;
        this.capPeso = 0.0;
        this.availability = false;
        this.mavailability = false;
        this.tmedico = false;
        this.registo = new HashMap<>();
        this.queue = new ArrayList<>();
        this.classificacoes = new ArrayList<>();
    }


    /**
     * Cópia
     */
    public Transportadora(Transportadora ede) {
        this.posicao = ede.getPosicao();
        this.codEmpresa = ede.getCodEmpresa();
        this.nomeEmpresa = ede.getNomeEmpresa();
        this.nif = ede.getNif();
        this.raio = ede.getRaio();
        this.ppkm = ede.getPrecoKm();
        this.kms = ede.getKms();
        this.vm = ede.getVM();
        this.capPeso = ede.getCapPeso();
        this.availability = ede.getAvailability();
        this.mavailability = ede.getMAvailability();
        this.tmedico = ede.getTMedico();
        this.registo = ede.getRegisto();
        this.queue = ede.getQueue();
        this.classificacoes = ede.getClassificacoes();
    }

    /**
     * Parametrizado
     */
    public Transportadora(double x, double y, String codEmpresa, String nomeEmpresa, String nif, double raio, double ppkm, double kms, double vm, double capPeso, boolean availability, boolean mavailability, boolean tmedico, Map<String, Encomenda> registo, List<Encomenda> queue, List<Float> css) {
        this.posicao = new Posicao(x,y);
        this.codEmpresa = codEmpresa;
        this.nomeEmpresa = nomeEmpresa;
        this.nif = nif;
        this.raio = raio;
        this.ppkm = ppkm;
        this.kms = kms;
        this.vm = vm;
        this.capPeso = capPeso;
        this.availability = availability;
        this.mavailability = mavailability;
        this.tmedico = tmedico;

        this.registo = new HashMap<>();
        for (Map.Entry<String, Encomenda> e : registo.entrySet()) {
            this.registo.put(e.getKey(), e.getValue());
        }

        this.queue = new ArrayList<>();
        for(Encomenda aQueue: queue){
            this.queue.add(aQueue);
        }

        this.classificacoes = new ArrayList<>();
        for (Float aFloat : css) {
            this.classificacoes.add(aFloat);
        }
    }


    /**
     * Getters
     */
    public Posicao getPosicao() {
        return posicao;
    }

    public String getCodEmpresa() {
        return codEmpresa;
    }

    public String getNomeEmpresa() {
        return nomeEmpresa;
    }

    public String getNif() {
        return nif;
    }

    public double getRaio() {
        return raio;
    }

    public double getPrecoKm() {
        return ppkm;
    }

    public double getKms() {
        return kms;
    }

    public double getVM() {
        return vm;
    }

    public double getCapPeso(){
        return capPeso;
    }

    public boolean getAvailability(){
        return availability;
    }

    public boolean getMAvailability(){
        return mavailability;
    }

    public boolean getTMedico(){
        return tmedico;
    }

    public Map<String, Encomenda> getRegisto() {
        return registo;
    }

    public List<Encomenda> getQueue() {
        return queue;
    }

    public List<Float> getClassificacoes(){
        return classificacoes;
    }


    /**
     * Setters
     */
    public void setPosicao(Posicao posicao) {
        this.posicao = posicao;
    }

    public void setCodEmpresa(String codEmpresa) {
        this.codEmpresa = codEmpresa;
    }

    public void setNomeEmpresa(String nomeEmpresa) {
        this.nomeEmpresa = nomeEmpresa;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public void setPrecoKm(double ppkm) {
        this.ppkm = ppkm;
    }

    public void setKms(double kms) {
        this.kms = kms;
    }

    public void setVM(double vm) {
        this.vm = vm;
    }

    public void setCapPeso(double c){
        this.capPeso = c;
    }

    public void setAvailability(boolean b){
        this.availability = b;
    }

    public void setMavailability(boolean mavailability) {
        mavailability = mavailability;
    }

    public void setTmedico(boolean tmedico) {
        this.tmedico = tmedico;
    }

    public void setRegisto(Map<String, Encomenda> registo) {
        this.registo = registo;
    }

    public void setQueue(List<Encomenda> queue) {
        this.queue = queue;
    }

    public void setClassificacoes(List<Float> f){
        this.classificacoes = f;
    }


    /**
     * Adds
     */
    public void addRegisto(Encomenda e){
        registo.put(e.getCodEncomenda(), e);
    }

    public void addQueue(Encomenda e){
        queue.add(e);
    }

    public void addkms(double kms){
        this.kms += kms;
    }




    /**
     * Funçao que passa o booleano de disponibilidade para string
     */
    public String btoString(boolean b){
        if(!b){
            return "Nao disponivel.";
        }
        else return "Disponivel.";
    }


    /**
     * Mudança de disponibilidade
     */
    public void mudaAvailability(){
        if(availability) availability = false;
        else availability = true;
    }

    /**
     * Funçao que aceita transportes de medicamentos
     */
    public boolean aceitoTransporteMedicamentos(){
        return this.mavailability;
    }

    /**
     * Funçao que muda a flag de aceita transportes de medicamentos
     */
    public void aceitaMedicamentos(boolean state){
        this.mavailability = state;
    }


    /**
     * Calcula media de classificaçoes
     */
    public float calculaCF(){
        float total = 0;

        for(int i = 0; i < classificacoes.size(); i++){
            total += classificacoes.get(i);
        }

        return total/classificacoes.size();
    }

    /**
     * Calcula tempo de deslocaçao
     */
    public double calculaTDesloc(double dist){
        return dist / vm;
    }

    /**
     * Funçao que calcula os portes de uma encomenda
     */
    public double calculaPortes(double dist){
        return dist * ppkm;
    }


    /**
     * Funçao que entrega encomenda
     */
    public Map<String, Double> fazerEntrega(SGE sge){
        Map<String, Double> ret = new HashMap<>();

        for(Encomenda e : queue){
            /*Loja da encomenda*/
            Loja l = sge.getLojas().get(e.getCodLoja());

            /*Utilizador da encomenda*/
            Utilizador u = sge.getUtilizadores().get(e.getCodUtilizador());

            /*Distancia loja utilizador*/
            double dist = l.getPosicao().distancia(u.getPosicao());

            /*Calculo de portes e associa a encomenda este preço de portes*/
            e.setPortes(calculaPortes(dist));

            /*Tempo de desloc*/
            double temp = calculaTDesloc(dist);

            /*Tempo da loja ate ao utilizador*/
            e.setIntervalo(temp);

            /*Encomenda entregue*/
            e.setEntregue(true);

            /*Add ao mapa*/
            ret.put(e.getCodEncomenda(), temp);

            /*Dar set a hora de entrega*/
            e.setDataDeChegada(LocalDateTime.now());

            /*Adiçao de kms a contagem da transportadora*/
            addkms(dist);
        }

        /*Remove da encomenda da queue*/
        queue = new ArrayList<>();

        return ret;
    }

    /**
     * Funçao que transforma a map de registo numa lista organizada por data de chegada
     */
    public List<Encomenda> encomendasToList(){
        List<Encomenda> ret = new ArrayList<>();

        for(Encomenda e : registo.values()){
            if(e.getEntregue()) ret.add(e);
        }

        Collections.sort(ret, new ComparatorEncomendaData());

        return ret;
    }




    /**
     * Manipulação
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Empresa: ").append(codEmpresa).append(",")
                .append("Nome Empresa: ").append(nomeEmpresa).append(",")
                .append("NIF: ").append(nif).append(",")
                .append("Preço por km: ").append(ppkm).append(",")
                .append("Numero kms percorridos: ").append(kms).append(",")
                .append("Capacidade de peso: ").append(capPeso).append(",")
                .append("Posicao: ").append(posicao).append(",")
                .append("Raio: ").append(raio).append(",")
                .append("Registo de Encomendas: ").append(registo.toString()).append(",")
                .append("Encomendas a entregar: ").append(queue.toString()).append(",")
                .append("Availability: ").append(availability).append(",")
                .append("Medical Availability: ").append(mavailability).append(",")
                .append("Medical Transport: ").append(tmedico).append(",")
                .append("Classificaçoes: ").append(classificacoes.toString());

        return sb.toString();
    }

    /**
     * Metodo Equals
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Transportadora that = (Transportadora) o;
        return that.kms == kms &&
                Double.compare(that.raio, raio) == 0 &&
                Double.compare(that.ppkm, ppkm) == 0 &&
                Double.compare(that.capPeso, capPeso) == 0 &&
                availability == that.availability &&
                mavailability == that.mavailability &&
                tmedico == that.tmedico &&
                Objects.equals(posicao, that.posicao) &&
                Objects.equals(codEmpresa, that.codEmpresa) &&
                Objects.equals(nomeEmpresa, that.nomeEmpresa) &&
                Objects.equals(nif, that.nif) &&
                Objects.equals(registo, that.registo) &&
                Objects.equals(queue, that.queue) &&
                Objects.equals(classificacoes, that.classificacoes);
    }

    /**
     * Metodo Hashcode
     */
    @Override
    public int hashCode() {
        return Objects.hash(posicao, codEmpresa, nomeEmpresa, nif, raio, ppkm, kms, capPeso, availability, mavailability, tmedico, registo, queue, classificacoes);
    }

    /**
     * Metodo Clone
     */
    public Transportadora clone() {
        return new Transportadora(this);
    }
}
