/**
 * 
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */

import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;

public abstract class Transporte implements Serializable{
    private String codT;
    private String nome;
    private double kms;
    private Coordenadas gps;
    private double raio;
    private double mediaclassificacao;
    private double velocidade;
    private boolean disponivel;
    private List<Encomenda> registoT; //Variável que regista todas as encomendas realizadas
    private boolean certificado; //Variável que testa se o voluntário/empresa têm certificado para transportar medicamentos;
    
    /**
     * Construtores
     */
    public Transporte(){
        this.codT = "N/a";
        this.nome = "N/a";
        this.kms = 0.0;
        this.gps = new Coordenadas();
        this.raio = 5;
        this.mediaclassificacao = 0;
        this.velocidade = 0.0;
        this.disponivel = true;
        this.registoT = new ArrayList<>();
        this.certificado = false;
    }
    
    public Transporte(String codT, String nome, double kms, Coordenadas gps, double raio, double mediaclassificacao,double velocidade, boolean disponivel,List<Encomenda> registoT, boolean certificado) {
        this.codT = codT;
        this.nome = nome;
        this.kms = kms;
        this.gps = gps;
        this.raio = raio;
        this.mediaclassificacao = mediaclassificacao;
        this.velocidade = velocidade;
        this.disponivel = disponivel;
        this.registoT = registoT;
        this.certificado = certificado;
    }

    public Transporte(Transporte t){
        this.codT = t.getCodT();
        this.nome = t.getNome();
        this.kms = t.getKms();
        this.gps = t.getGps();
        this.raio = t.getRaio();
        this.mediaclassificacao = t.getMediaClassificacao();
        this.velocidade = t.getVelocidade();
        this.disponivel = t.getDisponivel();
        this.registoT = t.getRegistoT();
        this.certificado = t.aceitoTransporteMedicamentos();
    }
    
    /**
     * Gets
     */

    public String getCodT() {
        return this.codT;
    }

    public String getNome() {
        return this.nome;
    }

    public double getKms() {
        return this.kms;
    }
    
    public Coordenadas getGps() {
        return this.gps;
    }

    public double getRaio(){
        return this.raio;
    }
    
    public double getMediaClassificacao(){
        return this.mediaclassificacao;
    }
    
    public double getVelocidade(){
        return this.velocidade;
    }
    
    public boolean getDisponivel(){
        return this.disponivel;
    }
    
    public List<Encomenda> getRegistoT(){
        ArrayList<Encomenda> copia = new ArrayList<>();
        for(Encomenda e: this.registoT){
            copia.add(e);
        }
        return copia;
    }

    /**
     * Sets
     */
    public void setCodT(String codT) {
        this.codT = codT;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }
    
    public void setKms(double kms) {
        this.kms = kms;
    }

    public void setGps(Coordenadas gps) {
        this.gps = gps;
    }
    
    public void setRaio(double raio){
        this.raio = raio;
    }
    
    public void setmediaclassificacao(double mediaclassificacao){
        this.mediaclassificacao = mediaclassificacao;
    }
    
    public void setVelocidade(double velocidade){
        this.velocidade = velocidade;
    }
    
    public void setDisponivel(boolean disp){
        this.disponivel = disp;
    }
    
    public void setRegistoT(List<Encomenda> registoT){
        this.registoT = registoT;
    }
    
    /**
     * clone
     */
    public abstract Transporte clone();

    /**
     * equals
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Transporte l = (Transporte) obj;
        return  l.getCodT().equals(this.codT);
    }
    
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código de Utilizador: ").append(this.codT).append("\n");
        sb.append("Nome: ").append(this.nome).append("\n");
        sb.append("Kms percorridos: ").append(this.kms).append("\n");
        sb.append("GPS: ").append(this.gps).append("\n");
        sb.append("Raio: ").append(this.raio).append("\n");
        sb.append("Media da Classifacao: ").append(this.mediaclassificacao).append("\n");
        sb.append("Disponivel: ").append(this.disponivel).append("\n");
        sb.append("Registo de todas as encomendas realizadas: ").append(this.registoT).append("\n");
        return sb.toString();
    } 
    
    public int compareTo(Transporte x){
        if (this.nome.equals(x.getNome()))
            return this.nome.compareTo(x.getNome());
        return this.codT.compareTo(x.getCodT());
    }
    
    /**
     * Obtem informaçoes acerca das encomendas efetuadas pelo transporte
     */
    public ArrayList<Encomenda> obteminfoencomendas(LocalDateTime hora1, LocalDateTime hora2){
        ArrayList<Encomenda> encomendas = new ArrayList<>();
        for (Encomenda e: this.registoT){
            if (e.getHora().isAfter(hora1) && e.getHora().isBefore(hora2)){
                encomendas.add(e);
            }
        }
        return encomendas;
    }
    
    /**
     * Classifica
     */
    public void classifica(int x){
        this.mediaclassificacao = (this.mediaclassificacao + x) / 2;
    }
    
    public boolean aceitoTransporteMedicamentos(){
        return this.certificado;
    }
    
    public void aceitaMedicamentos(boolean state){
        this.certificado = state;
    }
}
