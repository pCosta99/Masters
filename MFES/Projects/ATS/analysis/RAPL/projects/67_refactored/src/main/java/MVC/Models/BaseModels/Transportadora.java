package MVC.Models.BaseModels;

import java.io.Serializable;
import java.util.Random;
/**
 * Class para um User do tipo Transportadora e onde as funcionalidades
 * que este poderá usar.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */

public class Transportadora extends User implements Serializable {
    private String nif;
    private double raio;
    private double precoKm;
    private boolean estaLivre;
    private Classificacao classificacao;
    private double velocidadeMed;
    private double kmsTotal;
    private int capacidade;

    /**
     * Construtor Transportadora por defeito.
     */
    public Transportadora() {
        super();
        this.nif = "";
        this.raio = 0;
        this.precoKm = 0;
        this.classificacao = new Classificacao();
        this.estaLivre = true;
        this.velocidadeMed = 0;
        this.kmsTotal = 0;
        this.capacidade = 0;
    }

    /**
     * Construtor Transportadora parametrizado.
     * @param c Código da Transportadora.
     * @param n Nome da Transportadora.
     * @param x Coordenada X da Localização da Transportadora.
     * @param y Coordenada Y da Localização da Transportadora.
     * @param ni NIF da Transportadora.
     * @param r Raio de Entrega.
     * @param p Preço Por Km da Transportadora.
     */
    public Transportadora(String c, String n, double x, double y, String ni, double r, double p) {
        super(c, n, x, y);
        this.nif = ni;
        this.raio = r;
        this.precoKm = p;
        this.classificacao = new Classificacao();
        this.estaLivre = true;
        Random aux = new Random();
        this.velocidadeMed = (aux.nextInt(55)+35) + aux.nextDouble();
        this.kmsTotal = 0;
        this.capacidade = (new Random().nextInt(9)+1);
    }

    /**
     * Construtor Transportadora parametrizado.
     * @param c Código da Transportadora.
     * @param n Nome da Transportadora.
     * @param x Coordenada X da Localização da Transportadora.
     * @param y Coordenda Y da Localização da Transportadora.
     * @param ni NIF da Transportadora.
     * @param r Raio da Entrega.
     * @param p Preço por Km da Transportadora.
     * @param cap Capacidade da Transportadora.
     */
    public Transportadora(String c, String n, double x, double y, String ni, double r, double p, int cap) {
        super(c, n, x, y);
        this.nif = ni;
        this.raio = r;
        this.precoKm = p;
        this.classificacao = new Classificacao();
        this.estaLivre = true;
        Random aux = new Random();
        this.velocidadeMed = (aux.nextInt(55)+35) + aux.nextDouble();
        this.kmsTotal = 0;
        this.capacidade = cap;
    }

    /**
     * Construtor Transportadora por Cópia.
     * @param ts Transportadora a copiar.
     */
    public Transportadora(Transportadora ts) {
        super(ts);
        this.nif = ts.getNif();
        this.raio = ts.getRaio();
        this.precoKm = ts.getPrecoKm();
        this.classificacao = ts.getClassificacao();
        this.estaLivre = ts.getEstaLivre();
        this.velocidadeMed = ts.getVelocidadeMed();
        this.kmsTotal = ts.getKmsTotal();
        this.capacidade = ts.getCapacidade();
    }

    /**
     * Método que devolve a Capacidade da Transportadora.
     * @return Capacidade da Transportadora.
     */
    public int getCapacidade(){
        return this.capacidade;
    }

    /**
     * Método que decrementa a Capacidade da Transportadora.
     * Se a Capacidade após decrementar for 0, a Transportadora passa a não ficar livre.
     */
    public void decCapacidade(){
        this.capacidade--;
        if (this.capacidade == 0)
            this.estaLivre = false;
    }

    /**
     * Método que decrementa a Capacidade da Transportadora.
     * Se a Capacidade após incrementar for maior que 0, a Transportadora passa a ficar livre.
     */
    public void incCapacidade(){
        this.capacidade++;
        if (this.capacidade > 0)
            this.estaLivre = true;
    }

    /**
     * Método que verifica se uma Transportadora está livre para realizar Entregas.
     * @return True caso esteja livre, false caso contrário.
     */
    public boolean getEstaLivre() {
        return estaLivre;
    }

    /**
     * Método que define se uma Transportadora está livre para realizar encomendas.
     * @param estaLivre True para livre, false para o caso contrário.
     */
    public void setEstaLivre(boolean estaLivre) {
        this.estaLivre = estaLivre;
    }

    /**
     * Método que devolve a Classificação da Transportadora.
     * @return Classificação da Transportadora.
     */
    public Classificacao getClassificacao(){
        return this.classificacao.clone();
    }

    /**
     * Método que devolve o Rating da Transportadora.
     * @return Rating da Transportadora.
     */
    public double getNota() {
        return this.classificacao.getNota();
    }

    /**
     * Método que devolve o NIF da Transportadora.
     * @return NIF da Transportadora.
     */
    public String getNif() {
        return this.nif;
    }

    /**
     * Método que devolve o Raio de Entrega da Transportadora.
     * @return Raio de Entrega da Transportadora.
     */
    public double getRaio() {
        return this.raio;
    }

    /**
     * Método que devolve o Preço por Km para uma entrega de uma Transportadora.
     * @return Preço por Km da Transportadora.
     */
    public double getPrecoKm() {
        return this.precoKm;
    }

    /**
     * Método que devolve a Velocidade Média da Transportadora.
     * @return Velocidade Média da Transportadora.
     */
    public double getVelocidadeMed() {
        return velocidadeMed;
    }

    /**
     * Método que devolve o número de Kms feitos pela Transportadora.
     * @return Número de Kms Total.
     */
    public double getKmsTotal() {
        return this.kmsTotal;
    }

    /**
     * Método que adiciona um número de Kms de uma entrega ao Número de Kms Total.
     * @param kmsTotal Kms a adicionar.
     */
    public void addKmsTotal(double kmsTotal) {
        this.kmsTotal += kmsTotal;
    }

    /**
     * Método que adiciona uma Classificação à Transportadora.
     * @param nota Classificação dada.
     */
    public void classificaTransportadora(int nota){
        this.classificacao.addNota(nota);
    }

    /**
     * Método toString.
     * @return String que contém os dados da Transportadora.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Transportadora{ \n").append(super.toString()).append("\nNIF: ").append(this.getNif())
        .append("\nRaio: ").append(this.getRaio()).append("\nPreço por Km: ").append(this.getPrecoKm())
        .append("\nKms Total: ").append(String.format("%.2f",this.kmsTotal)).append("\nClassificacao:").append(String.format("%.2f",this.getNota())).append("\n}");
        return sb.toString();
    }

    /**
     * Método Clone.
     * @return Transportadora Clonada.
     */
    public Transportadora clone(){
        return new Transportadora(this);
    }
}