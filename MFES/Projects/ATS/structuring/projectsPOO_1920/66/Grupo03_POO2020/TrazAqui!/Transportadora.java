import java.io.*;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Classse com informações de uma transportadora
 */
public class Transportadora extends RegistoTULV {
    private static final double velocidade = 50.0;
    private double taxaPeso;
    private double precoKm;
    private boolean certMed;
    private double raio;
    private String nif;
    private double numKm;
    private List<Double> classificacao;


    /**
     * Construtor vazio
     */
    public Transportadora() {
        super();
        this.taxaPeso = 1;
        this.precoKm = 0;
        this.certMed = false;
        this.raio = 0;
        this.nif = "";
        this.numKm = 0;
        this.classificacao = new ArrayList<>();
    }

    /**
     * Construtor com argumentos
     * @param c Código
     * @param n Nome
     * @param p GPS
     * @param pass Password
     * @param tp Taxa Peso
     * @param pr Preço por Km
     * @param m Email
     * @param med Certificado médico
     * @param r Raio
     * @param nif NIF
     * @param mail Email
     * @param km Número de Kms
     * @param cl Classificação
     */
    public Transportadora(String c, String n, Ponto p, String pass, double tp, double pr, TreeMap<String, EncDistr> m, boolean med, double r, String nif, String mail, double km,ArrayList<Double> cl) {
        super(c,n,p,pass,m,mail);
        this.taxaPeso = tp;
        this.precoKm = pr;
        this.certMed = med;
        this.raio = r;
        this.nif = nif;
        this.numKm = km;
        this.classificacao = new ArrayList<>(cl);
    }

    /**
     * Construtor que recebe um objeto Transportadora
     * @param t Transportadora
     */
    public Transportadora(Transportadora t) {
        super(t);
        this.taxaPeso = t.taxaPeso;
        this.precoKm = t.precoKm;
        this.certMed = t.certMed;
        this.raio = t.raio;
        this.nif = t.nif;
        this.numKm = t.numKm;
        this.classificacao = new ArrayList<>(t.classificacao);
    }

    /**
     * Devolve taxa de peso
     * @return Taxa de peso
     */
    public double getTaxaPeso() { return taxaPeso;}

    /**
     * Devolve preço por km
     * @return Preço por km
     */
    public double getPrecoKm () { return this.precoKm; }

    /**
     * Devolve certificado médico
     * @return Certificado médico
     */
    public boolean getCertMed() { return certMed; }

    /**
     * Devolve raio
     * @return Raio
     */
    public double getRaio () { return this.raio; }

    /**
     * Deolve NIF
     * @return NIF
     */
    public String getNif() { return nif; }

    /**
     * Devolve número de kms
     * @return Número de kms
     */
    public double getNumKm () { return this.numKm; }

    /**
     * Devolve classificação
     * @return Classificação
     */
    public List<Double> getClassificacao() {
        return new ArrayList<>(this.classificacao);
    }

    /**
     * Devolve velocidade
     * @return Velocidade
     */
    public double getVelocidade() {return velocidade;}

    /**
     * Introduz taxa de peso
     * @param taxaPeso Taxa de peso
     */
    public void setTaxaPeso(double taxaPeso) { this.taxaPeso = taxaPeso; }

    /**
     * Introduz preço por km
     * @param c Preço por km
     */
    public void setPrecoKm (double c) { this.precoKm = c; }

    /**
     * Introduz certificado médico
     * @param certMed Certificado médico
     */
    public void setCertMed (boolean certMed) { this.certMed = certMed; }

    /**
     * Introduz raio
     * @param r Raio
     */
    public void setRaio (double r) { this.raio = r; }

    /**
     * Introduz NIF
     * @param nif NIF
     */
    public void setNif (String nif) { this.nif = nif; }

    /**
     * Introduz número de kms
     * @param r Número de kms
     */
    public void setNumKm (double r) { this.numKm = r; }
    public void setClassificacao(List<Double> classificacao) {
        this.classificacao = new ArrayList<>(classificacao);
    }

    /**
     * Método equals
     * @param o Transportadora
     * @return true se os objetos forem iguais ou false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Transportadora that = (Transportadora) o;
        return Double.compare(that.taxaPeso, taxaPeso) == 0 &&
                Double.compare(that.precoKm, precoKm) == 0 &&
                certMed == that.certMed &&
                Double.compare(that.raio, raio) == 0 &&
                Double.compare(that.numKm, numKm) == 0 &&
                Objects.equals(nif, that.nif);
    }

    /**
     * Método clone
     * @return Cópia de um objeto da classe Transportadora
     */
    public Transportadora clone () {
        return new Transportadora(this);
    }

    /**
     * Método toString
     * @return String com informações da transportadora
     */
    public String toString() {
        return ("\nT\n" +
                super.toString()
                + "Taxa Peso: " + this.taxaPeso + "\n"
                + "Preço por km: " + this.precoKm + "\n"
                + "Certificado médico: " + this.certMed) + "\n"
                + "Raio: " + this.raio + "\n"
                + "NIF: " + this.nif + "\n"
                + "Velocidade: " + velocidade + "\n"
                + "NumKm: " + numKm + "\n"
                + this.classificacao.toString() + "\n";
    }

    /**
     * Devolve certificado médico
     * @return true se aceitar transporte de medicamentos ou false caso contrário
     */
    public boolean aceitoTransporteMedicamentos() { return certMed; }

    /**
     * Calcula preço da viagem
     * @param l Loja
     * @param u Utilizador
     * @param e Encomenda
     * @return Preço
     */
    public double calculoPreco (Loja l , Utilizador u, Encomenda e) {
        double precoAteLoja = this.getGps().distancia(l.getGps()) * this.precoKm;
        double precoAteUser = l.getGps().distancia(u.getGps()) *  this.precoKm + e.getPeso() * this.getTaxaPeso();
        return precoAteLoja + precoAteUser;
    }

    /**
     * Calcula tempo de viagem
     * @param l Loja
     * @param u Utilizador
     * @return Tempo
     */
    public double calculoTempo (Loja l , Utilizador u){
        double tempoAteLoja = this.getGps().distancia(l.getGps()) / velocidade;
        double tempoAteUser = l.getGps().distancia(u.getGps()) / velocidade;
        double tempoNaLoja = l.getTempoMedioEspera();
        return tempoNaLoja + tempoAteLoja + tempoAteUser;
    }

    /**
     * Calcula distância
     * @param l Loja
     * @param u Utilizador
     * @return Distância
     */
    public double calculoDistancia (Loja l , Utilizador u){
        double distAteLoja = this.getGps().distancia(l.getGps());
        double distAteUtilizador = u.getGps().distancia(l.getGps());
        return distAteLoja + distAteUtilizador;
    }

    /**
     * Verifica se loja está no raio da transportadora
     * @param l Loja
     * @return true se estiver no raio ou false caso contrário
     */
    public boolean inRaio (Loja l) {
        double dist = this.getGps().distancia(l.getGps());
        return (dist<=this.getRaio());
    }

    /**
     * Adicona número de kms
     * @param km Kms
     */
    public void adicionaKm(double km) {
        this.numKm = this.numKm + km;
    }

    /**
     * Calcula a média das classificações
     * @return Média das calssificações
     */
    public double classificacaoMedia() {
        return (this.classificacao.stream().reduce(0.0, Double::sum)) / (this.classificacao.size());
    }

    /**
     * Adiciona classificação
     * @param a Classificação
     */
    public void addClassi (double a) {
        this.classificacao.add(a);
    }

    /**
     * Converte uma string com data para LocalDateTime
     * @param d Data em string
     * @return Data em LocalDateTime
     */
    public LocalDateTime stringToDate (String d) {
        String[] dateProperties = d.split("-");
        int year = Integer.parseInt(dateProperties[0]);
        int month = dateProperties.length > 1 ? Integer.parseInt(dateProperties[1]) : 1;
        int day = dateProperties.length > 2 ? Integer.parseInt(dateProperties[2]) : 1;
        return LocalDateTime.of(year, month, day, 0, 0, 0);
    }

    /**
     * Devolve faturação entre duas datas
     * @param d1 Data incial
     * @param d2 Data final
     * @param re Objeto registo
     * @return Faturação
     */
    public double faturacaoPeriodo (String d1, String d2, Registo re) {
        LocalDateTime t1 = stringToDate(d1);
        LocalDateTime t2 = stringToDate(d2);
        double fat = 0;
        TreeMap<String, RegistoTULV> r = re.getRegistos();
        for(EncDistr en : this.getEncomendas().values()) {
            LocalDateTime aux = en.getEncomenda().getDataE();
            if (aux.isAfter(t1) && aux.isBefore(t2)) {
                fat += calculoPreco((Loja) r.get(en.getEncomenda().getCodLoja()), (Utilizador) r.get(en.getEncomenda().getCodUtilizador()), en.getEncomenda());
            }
        }
        return fat;
    }

    /**
     * Calcula taxa de trânsito de acordo com a data
     * @param h Data
     * @return Taxa de trânsito
     */
    public double calculaTransito (LocalDateTime h) {
        Random rs = new Random();
        double taxa;
        if (h.getHour() > 9 && h.getHour() < 19) taxa = 1.50 + (0.50) * rs.nextDouble();
        else taxa =  1.0 + (0.30) * rs.nextDouble();
        return taxa;
    }

    /**
     * Calcula tempo até recolher encomenda da loja
     * @param l Loja
     * @param h Data
     * @return Tempo
     */
    public double calculoTempoAteRecolher (Loja l, LocalDateTime h){
        double tempoAteLoja = this.getGps().distancia(l.getGps()) / velocidade;
        double tempoNaLoja = l.getTempoMedioEspera();
        double taxa = calculaTransito(h);
        return (tempoNaLoja + tempoAteLoja)*taxa;
    }

    /**
     * Calcula tempo da loja ao utilizador
     * @param l Loja
     * @param u Utilizador
     * @param h Data
     * @return Tempo
     */
    public double calculoTempoAteUser (Loja l , Utilizador u, LocalDateTime h){
        double taxa = calculaTransito(h);
        return (l.getGps().distancia(u.getGps()) / velocidade)*taxa;
    }
}
