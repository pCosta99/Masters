package Model.Atores.Transportadores;

import Model.Atores.Ator;
import Model.Encomenda;

import java.awt.geom.Point2D;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import java.util.Objects;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Classe Transporte
 * @author grupo 115
 */

public  class Transporte extends Ator implements Serializable {


    /**
    * Variaveis de instancia
     */

    private boolean disponibilidade;
    private double raio;
    private boolean certeficado;
    private double classificacao;
    private int numeroEntregas;
    private double velocidadeMedia; //em km/h
    private double numeroKms;
    private Map<String, Encomenda> encomendas;

    /**
     * Construtor por omissão
     */
    public Transporte(){
        super();
        this.disponibilidade = true;
        this.raio = 0;
        this.certeficado = false;
        this.classificacao = 0;
        this.numeroEntregas = 0;
        this.velocidadeMedia = 0;
        this.numeroKms = 0;
        this.encomendas = new HashMap<>();


    }

    /**
     * Construtor parâmeterizado
     * @param email
     * @param referencia
     * @param nome
     * @param password
     * @param morada
     * @param nif
     * @param disponibilidade
     * @param raio
     * @param certeficado
     * @param classificacao
     * @param NumeroEntregas
     * @param VelocidadeMedia
     * @param nrKms
     * @param encomendas
     */
    public Transporte(String email, String referencia, String nome, String password, Point2D.Double morada, long nif, boolean disponibilidade, double raio, boolean certeficado, double classificacao, int NumeroEntregas , double VelocidadeMedia, double nrKms,Map<String,Encomenda> encomendas) {
        super(email,referencia,nome,password, morada, nif);
        this.disponibilidade = disponibilidade;
        this.raio = raio;
        this.certeficado = certeficado;
        this.classificacao = classificacao;
        this.numeroEntregas = NumeroEntregas;
        this.velocidadeMedia = VelocidadeMedia;
        this.numeroKms = nrKms;
        this.encomendas = encomendas;
    }

    /**
     * Construtor por cópia de objeto
     * @param a
     */
    public Transporte(Transporte a){
        super(a.getEmail(),a.getReferencia(),a.getNome(),a.getPassword(),a.getMorada(),a.getNif());
        this.disponibilidade = a.isDisponivel();
        this.raio = a.getRaio();
        this.certeficado = a.isCerteficado();
        this.classificacao = a.getClassificacao();
        this.numeroEntregas = a.getNumeroEntregas();
        this.velocidadeMedia = a.getVelocidadeMedia();
        this.numeroKms = a.getNumeroKms();
        this.encomendas = a.getEncomendas();

    }

    /**
     * getters e setters
     *
     */
    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> aux = new HashMap<>();
        for(Map.Entry<String,Encomenda> e:this.encomendas.entrySet())
            aux.put(e.getKey(),e.getValue());
        return aux;

    }


    public void setEncomendas(Map<String,Encomenda>enc){
        this.encomendas = new HashMap<>();
        enc.entrySet().forEach(e-> this.encomendas.put(e.getKey(),
                e.getValue().clone()));
    }


    public boolean isDisponivel() {
        return disponibilidade;
    }

    public double getRaio() {
        return raio;
    }

    public boolean isCerteficado() {
        return certeficado;
    }

    public double getClassificacao() {
        return classificacao;
    }

    public int getNumeroEntregas() {
        return numeroEntregas;
    }

    public double getVelocidadeMedia() {
        return velocidadeMedia;
    }


    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public void setCerteficado(boolean certeficado) {
        this.certeficado = certeficado;
    }


    public void setNumeroEntregas(int numeroEntregas) {
        this.numeroEntregas = numeroEntregas;
    }

    public double getNumeroKms() {
        return numeroKms;
    }

    public void setNumeroKms(double numeroKms) {
        this.numeroKms = numeroKms;
    }


    /**
     * Metodos
     *
     */



    public void adicionaEncomendaTransporte(Encomenda e) {
        this.encomendas.put(e.getReferencia(),e.clone());
    }

    public void aceitaEncomenda(Encomenda a) {
        setDisponibilidade(false);
    }


    public void setClassificacao(double classificacao) {

        double anterior = getClassificacao()*this.getNumeroEntregas();
        setNumeroEntregas(this.getNumeroEntregas()+1);
        this.classificacao = (classificacao+anterior)/this.getNumeroEntregas();
    }


    public double distancia (Encomenda a){
       return  a.getLoja().getMorada().distance(this.getMorada());

    }

    public boolean distanciaValida(Encomenda a){
        if (a.getLoja().getMorada().distance(this.getMorada())>getRaio()) return false;
        else return true;
    }


    public void addKms(Encomenda a){
        double b = this.getNumeroKms();
        b+=distancia(a)+this.getMorada().distance(a.getComprador().getMorada());
        this.setNumeroKms(b);

    }

    public double tempoViagem(Encomenda a) {

    if (this.getVelocidadeMedia() == 0) this.setVelocidadeMedia(ThreadLocalRandom.current().nextInt(30,80));
    double tempo = ((a.getLoja().getMorada().distance(this.getMorada())+a.getComprador().getMorada().distance(a.getLoja().getMorada()))*60)/this.getVelocidadeMedia();
    tempo+=a.getLoja().tempoTotalEspera();
        int atraso = ThreadLocalRandom.current().nextInt(0, 50);
        if (atraso>0 && atraso < 10){
            double c = tempo*(atraso/100);
            tempo += c;
            System.out.println("Ocorreu um atraso de " + c + " minutos devido a trafego intenso");
        }
        if (atraso>10 && atraso < 15){
            double c = tempo*(atraso/100);
            tempo += c;
            System.out.println("Ocorreu um atraso de " + c + " minutos devido ao clima");
        }

        return tempo;
    }

    public Map<String,Encomenda> getEncomendasPedidas(){
        Map<String,Encomenda> aux = new HashMap<>();
        for (Map.Entry<String,Encomenda> e : this.encomendas.entrySet())
            if(!e.getValue().isAceiteTransportador()) aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }


    public void removeEncomendaTransportador(Encomenda e) {
        this.encomendas.remove(e.getReferencia());
    }

    public void setVelocidadeMedia(double velocidadeMedia) {
        this.velocidadeMedia = velocidadeMedia;
    }

    @Override
    public String toString() {
        return "Transporte{" +
                "disponibilidade=" + disponibilidade +
                ", raio=" + raio +
                ", certeficado=" + certeficado +
                ", classificacao=" + classificacao +
                ", numeroEntregas=" + numeroEntregas +
                ", velocidadeMedia=" + velocidadeMedia +
                ", numeroKms=" + numeroKms +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Transporte)) return false;
        if (!super.equals(o)) return false;
        Transporte that = (Transporte) o;
        return disponibilidade == that.disponibilidade &&
                Double.compare(that.getRaio(), getRaio()) == 0 &&
                isCerteficado() == that.isCerteficado() &&
                Double.compare(that.getClassificacao(), getClassificacao()) == 0 &&
                getNumeroEntregas() == that.getNumeroEntregas() &&
                Double.compare(that.getVelocidadeMedia(), getVelocidadeMedia()) == 0 &&
                Double.compare(that.getNumeroKms(), getNumeroKms()) == 0 &&
                Objects.equals(getEncomendas(), that.getEncomendas());
    }

    @Override
    public int hashCode() {
        return Objects.hash(disponibilidade, getRaio(), isCerteficado(), getClassificacao(), getNumeroEntregas(), getVelocidadeMedia(), getNumeroKms(), getEncomendas());
    }

    public  Transporte clone(){
        return new Transporte(this);
    }


}
