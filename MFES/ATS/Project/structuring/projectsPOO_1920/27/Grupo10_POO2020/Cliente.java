/**
 * Escreva a descrição da classe CLiente aqui.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200517
 */

import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;

public class Cliente implements Comparable<Cliente>, Serializable{
    private int num_encomendas;
    private String codC;
    private String nome;
    private Coordenadas gps;
    private int classificacao;
    private List<Encomenda> encomendas;

    /**
     * Construtores
     */
    public Cliente(){
        this.num_encomendas = 0;
        this.codC = "N/a";
        this.nome = "N/a";
        this.gps = new Coordenadas();
        this.classificacao = 0;
        this.encomendas = new ArrayList<Encomenda>();
    }
    
    public Cliente(int num_encomendas, String codC, String nome, Coordenadas gps,int classificacao, List<Encomenda> encomenda){
        this.num_encomendas = num_encomendas;
        this.codC = codC;
        this.nome = nome;
        this.gps = gps;
        this.classificacao = classificacao;
        this.encomendas = encomenda;
    }

    public Cliente(Cliente c){
        this.num_encomendas = c.getNenc();
        this.codC = c.getCodC();
        this.nome = c.getNome();
        this.gps = c.getGps();
        this.classificacao = c.getClassificacao();
        this.encomendas = c.getEncomenda();
    }

    /**
     * Get's
     */
    public int getNenc() {
        return this.num_encomendas;
    }
    
    public String getCodC() {
        return this.codC;
    }

    public String getNome() {
        return this.nome;
    }

    public Coordenadas getGps() {
        return this.gps;
    }
    
    public int getClassificacao(){
        return this.classificacao;
    }
    
    public List<Encomenda> getEncomenda(){
        ArrayList<Encomenda> copia = new ArrayList<>();
        for(Encomenda e: this.encomendas){
            copia.add(e);
        }
        return copia;
    }

    /**
     * Set's
     */
    public void setNenc(int n_enc) {
        this.num_encomendas = n_enc;
    }
    
    public void setCodC(String codC) {
        this.codC = codC;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setGps(Coordenadas gps) {
        this.gps = gps;
    }
    
    public void setClassificacao(int classificacao){
        this.classificacao = classificacao;
    }
    
    public void setencomenda(List<Encomenda> encomenda){
        this.encomendas = encomenda;
    }
    
    /**
     * clone
     */
    public Cliente clone(){
        return new Cliente(this);
    }
    
    /**
     * equals
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Cliente l = (Cliente) obj;
        return  l.getCodC().equals(this.codC) &&
                l.getNome().equals(this.nome);
    }
  
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Numero de encomendas: ").append(this.num_encomendas);/*
        sb.append("Código de Utilizador: ").append(this.codC);
        sb.append("Nome: ").append(this.nome);
        sb.append("GPS: ").append(this.gps);
        sb.append("Classificacao: ").append(this.classificacao);
        sb.append("Encomendas: ").append(this.encomendas);*/
        return sb.toString();
    } 
    
    /**
     * 
     */
    public int compareTo(Cliente x){
        if (this.nome.equals(x.getNome()))
            return this.nome.compareTo(x.getNome());
        return this.codC.compareTo(x.getCodC());  
    }
    
    /**
     * Obtem informações sobre as encomendas efetuadas pelo cliente num determinada periodo de tempo
     */
    public ArrayList<Encomenda> obteminfoencomendas(LocalDateTime hora1, LocalDateTime hora2){
        ArrayList<Encomenda> encomendas = new ArrayList<>();
        for (Encomenda e: this.encomendas){
            if (e.getHora().isAfter(hora1) && e.getHora().isBefore(hora2)){
                encomendas.add(e);
            }
        }
        return encomendas;
    }
    
    /**
     * Adiciona uma encomenda à lista de encomendas já feitas
     */
    public void adicionaEncomenda(Encomenda encomenda){
        this.encomendas.add(encomenda);
    }
}
