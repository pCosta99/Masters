/**
 * Classe que representa os voluntarios
 */
package Modelo;

import java.io.Serializable;
import java.util.Set;
import java.util.TreeSet;

public class Voluntario implements Serializable {
    private String codVoluntario;
    private String nome ;
    private Gps gps;
    private double raio;
    private Set<String> encomendas ;
    private String pass;
    private boolean disponivel;
    private boolean medico;

    //CONTRUTORES

    /**
     * Contrutor vazio
     */
    public Voluntario(){
        this.codVoluntario = "Invalid";
        this.nome = "Invalid";
        this.gps = new Gps();
        this.raio = 0;
        this.encomendas = new TreeSet<>();
        this.pass = "0000";
        this.disponivel = true;
        this.medico = false;
    }

    /**
     * Contrutor parametrizado
     * @param codVoluntario codigo para identificar o voluntario
     * @param nome nome do voluntario
     * @param gps cordenadas do voluntario
     * @param raio raio do voluntario
     * @param encomendas historico de encomendas do voluntario
     * @param pass password do voluntario
     * @param disponivel Boolean se o volutario esta disponivel ounão
     * @param medico se o voluntairo pode transportar medicamentos ou não
     */
    public Voluntario(String codVoluntario, String nome, Gps gps, double raio, Set<String> encomendas , String pass, boolean disponivel , boolean medico) {
        this.codVoluntario = codVoluntario;
        this.nome = nome;
        this.gps = gps.clone();
        this.raio = raio;
        this.encomendas = new TreeSet<>();
        for (String s : encomendas) {
            this.encomendas.add(s);
        }
        this.pass = pass;
        this.disponivel = disponivel;
        this.medico = medico;
    }

    /**
     * Contrutor de cópia
     * @param outro
     */
    public Voluntario(Voluntario outro){
        this.codVoluntario = outro.getCodVoluntario();
        this.nome = outro.getNome();
        this.gps = outro.getGps();
        this.raio = outro.getRaio();
        this.encomendas = new TreeSet<>();
        for (String s : outro.encomendas) {
            this.encomendas.add(s);
        }
        this.pass = outro.getPass();
        this.disponivel = outro.isDisponivel();
        this.medico = outro.isMedico();
    }

    /**
     * Método que adiciona uma encomenda ao historico de encomendas do voluntario
     * @param encCode
     */
    public void addEncomenda (String encCode){
        this.encomendas.add(encCode);
    }

    /**
     * Método que devolve um boolean que determina se o voluntario esta disponivel ou não
     * @return
     */
    public boolean isDisponivel() {
        return this.disponivel;
    }

    /**
     * Método que devolve um boolean que determina se o voluntario consegue ou não entregar enomendas medicas
     * @return
     */
    public boolean isMedico (){
        return this.medico;
    }

    /**
     * Método que modifica o estado de disponiblidade do voluntario
     * @param disponivel
     */
    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    /**
     * Retorna a lista de encomendas
     * @return Lista de encomendas
     */
    public Set<String> getEncomendas(){
        return new TreeSet<>(this.encomendas);
    }
    /**
     * Método que devolve o codigo do voluntario
     * @return
     */
    public String getCodVoluntario() {
        return this.codVoluntario;
    }

    /**
     * Método que modifica o codigo do ovluntario
     * @param codVoluntario
     */
    public void setCodVoluntario(String codVoluntario) {
        this.codVoluntario = codVoluntario;
    }

    /**
     * Método que devolve o nome do voluntario
     * @return
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * Método que define o nome de um voluntario
     * @param nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método que devolve as cordenadas gps um voluntario
     * @return
     */
    public Gps getGps() {
        return this.gps.clone();
    }

    /**
     * Método que modifica as cordenadas gps de um voluntario
     * @param gps
     */
    public void setGps(Gps gps) {
        this.gps = gps.clone();
    }

    /**
     * Método que devolve o raio de um voluntario
     * @return
     */
    public double getRaio() {
        return this.raio;
    }

    /**
     * Método que define o raio de um voluntario
     * @param raio
     */
    public void setRaio(double raio) {
        this.raio = raio;
    }

    /**
     * Metodo que modifica a passe de um voluntario
     * @param passe
     */
    public void setPass(String passe){ this.pass = passe;}

    /**
     * método que devolve a pass de um voluntario
     * @return
     */
    public String getPass(){ return this.pass; }

    /**
     * Método equals
     * @param obj
     * @return
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Voluntario a = (Voluntario) obj;
        return this.codVoluntario.equals(a.getCodVoluntario()) && this.nome.equals(a.getNome())
                && this.gps.equals(a.getGps()) && this.raio == a.getRaio();
    }

    /**
     * método clone
     * @return
     */
    @Override
    public Voluntario clone() {
        return new Voluntario(this);
    }

    /**
     * Método toString
     * @return
     */
    @Override
    public String toString() {
        return "Código de voluntário: " + this.codVoluntario + " Nome: " + this.nome + " " + this.gps.toString()
                + " Raio: " + this.raio + "\n";
    }

    /**
     * Método toString do historico do voluntario
     * @return
     */
    public String encomendasToString(){
        return "Encomendas: \n" + this.encomendas.toString();
    }

    public void removeEncomenda (String encCode){
        this.encomendas.remove(encCode);
    }

}
