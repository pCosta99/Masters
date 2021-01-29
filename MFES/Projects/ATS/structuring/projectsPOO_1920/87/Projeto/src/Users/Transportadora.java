package Users;

import Geral.GPS;
import Stock.Encomenda;
import Stock.EncomendaRealizadaTransportadora;
import Stock.EncomendaRealizadaVoluntario;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


public class Transportadora extends User implements  Serializable {

    private String nome;
    private GPS gps;
    private String nif;
    private double raio;
    private double precoPorKM;
    private boolean disponivel;
    private int encomendas_maximas;
    private List<EncomendaRealizadaTransportadora> register;
    private List<Encomenda> on_hold;
    private double classificação;
    private int total_aval;
    private int total_entregas;
    private int km_total;

    /**
     * Construtor por omissão.
     */
    public Transportadora() {
        super();
        this.nif = new String();
        this.raio = 0;
        this.precoPorKM = 0;
        this.disponivel = true;
        this.encomendas_maximas = 0;
        this.register = new ArrayList<>();
        this.on_hold = new ArrayList<>();
        this.classificação = 0;
        this.total_entregas = 0;
        this.total_aval = 0;
        this.km_total = 0;
    }

    /**
     * Construtor por parâmetros.
     * @param cE Código da Transportadora.
     * @param nE Nome da Transportadora.
     * @param password Password da Transportadora.
     * @param l Localização GPS da Transportadora.
     * @param n NIF da Transportadora.
     * @param r Raio de ação da Transportadora.
     * @param ppk Preço por quilómetro da Transportadora.
     * @param em Número de encomendas máximas da Transportadora.
     */
    public Transportadora(String cE, String nE,String password, GPS l, String n, double r, double ppk, int em) {
        super(cE, password);
        this.nome = nE;
        this.gps = l;
        this.nif = n;
        this.raio = r;
        this.precoPorKM = ppk;
        this.disponivel = true;
        this.encomendas_maximas = em;
        this.register = new ArrayList<>();
        this.on_hold = new ArrayList<>();
        this.classificação = 0;
        this.total_entregas = 0;
        this.total_aval = 0;
        this.km_total = 0;
    }

    /**
     * Construtor por clonagem.
     * @param t Transportadora a clonar.
     */
    public Transportadora(Transportadora t) {
        super(t);
        this.nome = t.getNome();
        this.on_hold = t.getOnHold();
        this.gps = t.getGPS();
        this.nif = t.getNIF();
        this.raio = t.getRaio();
        this.precoPorKM = t.getPPK();
        this.disponivel = t.getDisponibilidade();
        this.encomendas_maximas = t.getNMaximo();
        this.register = t.getRegisto();
        this.classificação = t.getClassificação();
        this.total_entregas = t.getTotal_entregas();
        this.total_aval = t.getTotalAval();
        this.km_total  = t.getTotalKM();
    }

    /**
     * Método que define o código de uma Transportadora.
     * @param code que é o código.
     */
    public void setCodigo(String code){
        super.setCode(code);
    }

    /**
     * Método que devolve o NIF de uma Transportadora.
     * @return NIF da Transportadora.
     */
    public String getNIF() {
        return nif;
    }

    /**
     * Método que devolve o nome de uma Transportadora.
     * @return Nome da Transportadora.
     */
    public String getNome(){
        return this.nome;
    }

    /**
     * Método que devolve a localização GPS de uma Transportadora.
     * @return Localização GPS.
     */
    public GPS getGPS(){
        return this.gps.clone();
    }

    /**
     * Método que devolve o raio de ação de uma Transportadora.
     * @return Raio de ação.
     */
    public double getRaio() {
        return raio;
    }

    /**
     * Método que define o raio de ação de uma Transportadora.
     * @param raio que é o raio de ação.
     */
    public void setRaio(double raio) {
        this.raio = raio;
    }

    /**
     * Método que devolve o preço por quilómetro de uma Transportadora.
     * @return Preço por quilóemtro.
     */
    public double getPPK() {
        return precoPorKM;
    }

    /**
     * Método que define o preço por quilómetro da Transportadora.
     * @param precoPorKM que é o preço por quilómetro.
     */
    public void setPPK(double precoPorKM) {
        this.precoPorKM = precoPorKM;
    }

    /**
     * Método que devolve o número máximo de encomendas que uma Transportadora é capaz de realizar.
     * @return Número máximo de encomendas.
     */
    public int getNMaximo() {
        return encomendas_maximas;
    }

    /**
     * Método que define o nome da Transportadora.
     * @param nome que é o nome da Transportadora.
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método que define a localização GPS de uma Transportadora.
     * @param gps que é a localização GPS.
     */
    public void setGps(GPS gps) {
        this.gps = gps.clone();
    }

    /**
     * Método que define o máximo de encomendas que uma Transportadora é capaz de realizar
     * @param encomendas_maximas que é o total de encomendas máximas.
     */
    public void setNMaximo(int encomendas_maximas) {
        this.encomendas_maximas = encomendas_maximas;
    }

    /**
     * Método que verifica a disponibilidade de uma Transportadora em realizar encomendas.
     * @return Boolean com a disponibilidade.
     */
    public boolean getDisponibilidade() {
        return disponivel;
    }

    /**
     * Método que devolve a lista de encomendas realizadas por uma Transportadora.
     * @return Lista com as encomendas realizadas.
     */
    public List<EncomendaRealizadaTransportadora> getRegisto() {
        return this.register.stream().map(EncomendaRealizadaTransportadora::clone).collect(Collectors.toList());
    }

    /**
     * Método que define o registo de encomendas realizadas por uma Transportadora.
     * @param l que é a lista com as encomendas realizadas.
     */
    public void setRegisto(List<EncomendaRealizadaTransportadora> l) {
        this.register = new ArrayList<>();
        l.forEach(e -> this.register.add(e.clone()));
    }

    /**
     * Método que define a disponibilidade de uma Transporadora.
     * @param disponivel que é a disponibilidade.
     */
    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    /**
     * Método que define o NIF de uma Transportadora.
     * @param nif que é o NIF.
     */
    public void setNif(String nif) {
        this.nif = nif;
    }

    /**
     * Método que devolve a classificação de uma Empresa Transportadora.
     * @return Classifcação.
     */
    public double getClassificação() {
        return classificação;
    }

    /**
     * Método que define a classificação de uma Empresa Transportadora.
     * @param classificação que é a classificação.
     */
    public void setClassificação(double classificação) {
        this.classificação = classificação;
    }

    /**
     * Método que devolve o total de entregas realizadas por uma Empresa Transportadora.
     * @return Total de entregas.
     */
    public int getTotal_entregas() {
        return total_entregas;
    }

    /**
     * Método que define o total de vendas realizadas por uma Empresa Transportadora.
     * @param total_entregas que é o número total de entregas.
     */
    public void setTotal_entregas(int total_entregas) {
        this.total_entregas = total_entregas;
    }

    /**
     * Método que devolve o total de avaliações de uma Transportadora.
     * @return Total de avaliações.
     */
    public int getTotalAval() {
        return total_aval;
    }

    /**
     * Método que define o total de avaliações de uma Transpotadora.
     * @param total_aval que é o total de avaliações da Transportadora.
     */
    public void setTotalAval(int total_aval) {
        this.total_aval = total_aval;
    }

    /**
     * Método que atualiza a classificação de uma Transportadora.
     * @param classifica nova classificação feita à Transpotadora.
     */
    public void updateClassificacao(int classifica){
        this.total_aval++;
        this.classificação = (this.classificação + classifica)/this.total_aval;
    }

    /**
     * Método que calcula o total faturado com custos de transporte num determinado período.
     * @param i  Data de início.
     * @param f Data de fim.
     * @return Valor faturado com os custos de transporte.
     */
    public double calculaFat(LocalDateTime i, LocalDateTime f){
        double aux = 0;
        for(EncomendaRealizadaTransportadora e : this.register){
            if(e.getData_entrega().isAfter(i) && e.getData_entrega().isBefore(f)){
                aux += e.getCusto_transporte();
            }
        }
        return aux;
    }

    /**
     * Método que devolve uma lista com encomendas à espera de aceitação do utilizador.
     * @return Lista com as encomendas.
     */
    public List<Encomenda> getOnHold(){
        return this.on_hold.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    /**
     * Método que define a lista de encomendas a espera de resposta do utilizador.
     * @param a Lista com as encomendas.
     */
    public void setOnHold(List<Encomenda> a){
        a.stream().map(Encomenda::clone).forEach(v -> this.on_hold.add(v));
    }

    /**
     * Método que adiciona uma enconenda realizada ao registo.
     * @param c_enc que é o código da encomenda.
     * @param c_util que é o código do utilizador que comprou.
     * @param loja que é a loja a quem comprou.
     * @param te que é o tempo que demorou a realizar a entregar.
     */
    public void addEncomendaRealizada(String c_enc, String c_util, String loja, double te,double peso, double preco,double dist){
        EncomendaRealizadaTransportadora nova = new EncomendaRealizadaTransportadora(c_enc,c_util,loja,te,peso*0.23+dist*this.precoPorKM,preco + dist*this.precoPorKM + peso*0.23,LocalDateTime.now());
        this.km_total += dist;
        this.register.add(nova);
        this.total_entregas++;
    }

    /**
     * Método que adiciona uma encomenda a lista de espera por aceitação de um utilizador.
     * @param e Encomenda.
     */
    public void addEncomendaParaAceitar(Encomenda e){
        this.on_hold.add(e.clone());
    }

    /**
     * Método que devolve o total de quilómetros que uma Transportadora percorreu.
     * @return Total de quilóemtros.
     */
    public int getTotalKM() {
        return km_total;
    }

    /**
     * Métodon que define o total de quilómetros percorridos por uma Transportadora.
     * @param km_total que é o tatal de quilóemtros percorridos.
     */
    public void setKm_total(int km_total) {
        this.km_total = km_total;
    }


    //Equals, toString , clone

    /**
     * Método que faz uma cópia de uma Transportadora.
     * @return Cópia.
     */
    public Transportadora clone() {
        return new Transportadora(this);
    }

    /**
     * Método que averiguar a igualdde entre objetos.
     * @param obj Que é o objeto a ser comparado.
     * @return boolean com resultado da comparação.
     */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Transportadora t = (Transportadora) obj;
        return (this.getCode().equals(t.getCode())
                && this.getNome().equals(t.getNome())
                && this.nif.equals(t.getNIF())
                && this.getGPS().equals(t.getGPS())
                && this.precoPorKM == t.getPPK()
                && this.raio == t.getRaio()
                && this.encomendas_maximas == t.getNMaximo()
                && this.disponivel == t.getDisponibilidade()
                && this.register.equals(t.getRegisto())
                && this.classificação == t.getClassificação()
                && this.total_entregas == t.getTotal_entregas());
    }


    /**
     * Método que converte numa string a informação sobre a Transportadora.
     * @return String com a informação da Transportadora.
     */
    public String toString(){
        return "Transportadora:"+this.getCode()+","+this.getNome()+","+this.getGPS().toString()+","+this.getNIF()+","+this.getRaio()+","+this.getPPK();
    }
}
