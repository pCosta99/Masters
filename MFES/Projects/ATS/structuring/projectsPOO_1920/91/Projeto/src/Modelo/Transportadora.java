/**
 * Classe que representa a empresa que transporta as encomendas para o utilizador
 */
package Modelo;

import java.io.Serializable;
import java.util.Set;
import java.util.TreeSet;

public class Transportadora implements Comparable<Transportadora>, Serializable {
    private String codEmpresa;
    private String nomeEmpresa;
    private Gps gps = new Gps();
    private String nif;
    private double raio;
    private double precoKm;
    private double precokg;
    private String password;
    private boolean medico;
    private boolean disponivel;
    private Set<String> entregues;
    private double rating;
    private int numratings;

    //CONTRUTORES

    /**
     * Construtor vazio
     */
    public Transportadora (){
        this.codEmpresa = "Invalid";
        this.nomeEmpresa = "Invalid";
        this.gps = new Gps();
        this.nif = "Invalid";
        this.raio = 0;
        this.precoKm = 0;
        this.password = "Invalid";
        this.precokg = 0;
        this.medico = false;
        this.entregues = new TreeSet<>();
        this.rating = 0;
        this.numratings = 0;
    }
    /** Construtor Paramatrizado
     *
     * @param codEmpresa cod empresa
     * @param nomeEmpresa nome empresa
     * @param gps gps
     * @param nif nif
     * @param raio raio
     * @param precoKm preço por km
     * @param password pass
     * @param precokg preço por kg
     * @param medico medico
     * @param disponivel disponivel
     * @param entregues entregues
     * @param rating rating
     * @param numratings numrating
     */
    public Transportadora(String codEmpresa, String nomeEmpresa, Gps gps, String nif, double raio, double precoKm,
                          String password, double precokg , boolean medico , boolean disponivel , Set<String> entregues, double rating, int numratings) {
        this.codEmpresa = codEmpresa;
        this.nomeEmpresa = nomeEmpresa;
        this.gps = gps.clone();
        this.nif = nif;
        this.raio = raio;
        this.precoKm = precoKm;
        this.password = password;
        this.precokg = precokg;
        this.medico = medico;
        this.disponivel = disponivel;
        this.entregues = new TreeSet<>(entregues);
        this.rating = rating;
        this.numratings = numratings;
    }

    /**Construtor de cópia
     *
     * @param outro
     */
    public Transportadora (Transportadora outro){
        this.codEmpresa = outro.getCodEmpresa();
        this.nomeEmpresa = outro.getNomeEmpresa();
        this.gps = outro.getGps().clone();
        this.nif = outro.getNif();
        this.raio = outro.getRaio();
        this.precoKm = outro.getPrecoKm();
        this.password = outro.getPassword();
        this.precokg = outro.getPrecokg();
        this.medico = outro.isMedico();
        this.disponivel = outro.isDisponivel();
        this.entregues = new TreeSet<>(outro.getEntregues());
        for (String s : outro.entregues ) {
            this.entregues.add(s);
        }
        this.rating = outro.rating;
        this.numratings = outro.numratings;
    }

    /**
     * Método que adiciona uma encomenda a lista de encomendas
     * @param codEnc codigo encomenda
     */
    public void addEntregues(String codEnc){
        this.entregues.add(codEnc);
    }

    //SETTERS E GETTERS

    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    /**
     * Método que devolve true se a empresa esta disponivel para fazer entregas, false se não estiver disponivel
     * @return boolean
     */
    public boolean isDisponivel() { return this.disponivel;}

    /**
     * Método que torna a transportadora capaz de transportar medicamentos se receber um true, ou o contrario se receber um false
     * @param medico boolean
     */
    public void setMedico(boolean medico) {
        this.medico = medico;
    }

    /**
     * Método que devolve true se a transportaadora for capaz de transportar medicamentos, e um false se não for
     * @return boolean
     */
    public boolean isMedico() {
        return this.medico;
    }

    /**
     * Método que devolve o codigo da empresa
     * @return cod empresa
     */
    public String getCodEmpresa() {
        return this.codEmpresa;
    }

    /**
     * Método que modifica o codigo da empresa
     * @param codEmpresa cod empresa
     */
    public void setCodEmpresa(String codEmpresa) {
        this.codEmpresa = codEmpresa;
    }

    /**
     * Métpdp que recebe o nome da empresa
     * @return nome empresa
     */
    public String getNomeEmpresa() {
        return this.nomeEmpresa;
    }

    /**
     * Método que modifica o nome da empresa
     * @param nomeEmpresa nome empresa
     */
    public void setNomeEmpresa(String nomeEmpresa) {
        this.nomeEmpresa = nomeEmpresa;
    }

    /**
     * Método que devolve as cordenadas de gps da empresa
     * @return gps
     */
    public Gps getGps() {
        return this.gps.clone();
    }

    /**
     * Método que modifica as cordenadas gps da empresa
     * @param gps gps
     */
    public void setGps(Gps gps) {
        this.gps = gps.clone();
    }

    /**
     * Método que devolve o Nif da empresa
     * @return nif
     */
    public String getNif() {
        return this.nif;
    }

    /**
     * Método que define o Nif da empresa
     * @param nif nif
     */
   public void setNif(String nif) {
        this.nif = nif;
    }

    /**
     * Método que devolve o raio de ação da transportadora
     * @return raio
     */
    public double getRaio() {
        return this.raio;
    }

    /**
     * Método que define o raio de ação da transportadora
     * @param raio raio
     */

    public void setRaio(double raio) {
        this.raio = raio;
    }

    /**
     * Método que devolve o preço por km
     * @return preço/km
     */
    public double getPrecoKm() {
        return this.precoKm;
    }

    /**
     * Método que define o preço por km
     * @param precoKm preço/km
     */

    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }

    /**
     * Método que devolve o preço por Kg
     * @return preço/kg
     */

    public double getPrecokg() {
        return this.precokg;
    }

    /**
     * Método que define o preço por kg
     * @param precoKg
     */
    public void setPrecokg(double precoKg) {
        this.precokg = precokg;
    }

    /**
     * Método que devolve a password
     * @return password
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Método que define a nova password
     * @param password password
     */
    public void setPassword(String password) { this.password = password; }

    /**
     * Método que devolve o historico de encomendas da empresa
     * @return historica encomenda
     */
    public Set<String> getEntregues() {
        return new TreeSet<>(this.entregues);
    }

    /**
     * Método que define o novo rating
     * @param rating rating
     */
    public void setRating(Double rating) {this.rating = rating;}

    /**
     * Método que devolve o rating da empresa
     * @return rating
     */
    public double getRating(){return rating;}

    /**
     * Método que define o número de ratings da empresa(para a média)
     * @param numratings num rating
     */
    public void setNumratings(int numratings) {this.numratings = numratings; }

    /**
     * Método que devolve o número de ratings da empresa
     * @return numero de ratings
     */
    public int getNumratings(){return numratings;}

    /**
     *
     * @param obj
     * @return
     */
    @Override

    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return true;
        Transportadora a = (Transportadora) obj;
        return this.codEmpresa.equals(a.getCodEmpresa())
                && this.nomeEmpresa.equals(a.getNomeEmpresa())
                && this.gps.equals(a.getGps())
                && this.nif.equals(a.getNif())
                && this.raio == a.getRaio()
                && this.precoKm == a.getPrecoKm()
                && this.precokg == a.getPrecokg()
                && this.medico == a.isMedico();
    }

    @Override
    public Transportadora clone() {
        return new Transportadora(this);
    }

    @Override
    public String toString() {
        return "Código do Empresa: " + this.codEmpresa + " Nome da Empresa: " + this.nomeEmpresa + " " +
                 this.gps.toString() + " Nif: " + this.nif + "Raio: " + this.raio + " Preço por Km: " + this.precoKm +  "Preço por Kg" + this.precokg + "\n";
    }

    //METODOS ADICIONAIS

    /**
     * Método que modifica a classificação do rating de acordo com a pontuação do ultimo rating de um Utilizador
     * @param rating do utilizador
     * @return novo rating
     */
    public double classificacao( int rating){
        this.numratings++;
        double newrating =(this.rating*(this.numratings-1)+rating)/numratings;
        this.setRating(newrating);
        return newrating;
    }

    @Override
    public int compareTo(Transportadora transportadora) {
        return transportadora.getEntregues().size() - this.getEntregues().size();
    }
}
