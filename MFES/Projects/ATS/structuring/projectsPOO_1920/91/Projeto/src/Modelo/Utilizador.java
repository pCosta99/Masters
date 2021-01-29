/**
 * Classe que representa o utilizador
 */
package Modelo;

import java.io.Serializable;
import java.util.*;

public class Utilizador implements Comparable<Utilizador>, Serializable {
    private String codUtilizador ;
    private String nome;
    private Gps gps = new Gps();
    private String password;
    private Set<String> encomendas;

    /**
     * Construtor vazio
     */
    public Utilizador(){
        this.codUtilizador = "Invalid";
        this.nome = "Invalid";
        this.gps = new Gps();
        this.password = "Invalid";
        this.encomendas = new TreeSet<>();
    }

    /**
     * Construtor parametrizado
     * @param codUtilizador
     * @param nome
     * @param gps
     * @param pass
     * @param encomendas
     */
    public Utilizador(String codUtilizador, String nome, Gps gps, String pass, Set<String> encomendas) {
        this.codUtilizador = codUtilizador;
        this.nome = nome;
        this.gps = gps.clone();
        this.password = pass;
        this.encomendas = new TreeSet<>();
        for (String s :
                encomendas ) {
            this.getEncomendas().add(s);
        }
    }

    /**
     * Construtor copia
     * @param outro
     */
    public Utilizador(Utilizador outro){
        this.codUtilizador = outro.getCodUtilizador();
        this.nome = outro.getNome();
        this.gps = outro.getGps().clone();
        this.password = outro.getPassword();
        this.encomendas = new TreeSet<>();
        for (String s :
                outro.getEncomendas()) {
            this.encomendas.add(s);
        }
    }

    /**
     * Método que devolve o codigo do utilizador
     * @return codigo de utilizador
     */
    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    /**
     * Método que modifica o codigo de utilizador
     * @param codUtilizador
     */
    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    /**
     * Método que devolve o Nome
     * @return nome
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * Método que modifica o nome
     * @param nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método que devolve cordenadas gps
     * @return gps
     */
    public Gps getGps() {
        return this.gps.clone();
    }

    /**
     * Método que modifica gps
     * @param gps cordenadas
     */
    public void setGps(Gps gps) {
        this.gps = gps.clone();
    }

    /**
     * Método que devolve o gps
     * @return gps
     */
    public String getPassword(){
        return this.password;
    }

    /**
     * Método que modifica a password
     * @param pass nova password
     */
    public void setPassword(String pass){
        this.password = pass;
    }

    /**
     * Método que devolve encomendas
     * @return Set<String> Encomendas
     */
    public Set<String> getEncomendas(){
        return new TreeSet<>(encomendas);
    }

    /**
     * Método que adiciona encomendas
     * @param codEncomenda
     */
    public void addEncomenda (String codEncomenda){
        this.encomendas.add(codEncomenda);
    }

    /**
     * Método equals
     * @param obj
     * @return boolean
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Utilizador a = (Utilizador) obj;
        return this.codUtilizador.equals(a.getCodUtilizador()) && this.nome.equals(a.getNome())
                && this.gps.equals(a.getGps()) && this.encomendas.equals(a.encomendas);
    }

    /**
     * Método clone
     * @return utilizador
     */
    @Override
    public Utilizador clone()  {
        return new Utilizador(this);
    }

    /**
     * Método toString
     * @return String
     */
    @Override
    public String toString() {
        return "Código: " + this.codUtilizador + " Nome de utilizador: " + this.nome + " "+ this.gps.toString() + "\n";
    }

    public int compareTo(Utilizador o) {
        return o.getEncomendas().size() - this.encomendas.size();
    }
}
