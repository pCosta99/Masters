package Model;

import java.util.Set;
import java.util.TreeSet;

public class Voluntario extends AtorSistema implements java.io.Serializable{

    private double raio;
    private double classificacao;
    private int numClassificacao;
    private boolean aceitaMedicas;
    private double velocidadeMedia;

    /**
     * Construtor parametrizado
     * @param raio
     * @param classificacao
     * @param numClassificacao
     */
    public Voluntario(String cod, String email,String nif, String nome, String password,
                      Coordenadas localizacao, double raio, double classificacao, int numClassificacao, boolean aceitaMedicas, double velocidade){
        super(cod,email,nif,nome,password,localizacao);
        this.raio = raio;
        this.classificacao = classificacao;
        this.numClassificacao = numClassificacao;
        this.aceitaMedicas = aceitaMedicas;
        this.velocidadeMedia = velocidade;
    }

    /**
     * Construtor parametrizado sem classificacao e numClassificacao
     * @param cod
     * @param email
     * @param nif
     * @param nome
     * @param password
     * @param localizacao
     * @param raio
     */
    public Voluntario(String cod, String email,String nif, String nome, String password,
                      Coordenadas localizacao, double raio, double velocidade){
        super(cod,email,nif,nome,password,localizacao);
        this.raio = raio;
        this.classificacao = 0;
        this.numClassificacao = 0;
        this.aceitaMedicas = false;
        this.velocidadeMedia = velocidade;
    }

    /**
     * Construtor por cópia
     * @param voluntario
     */
    public Voluntario(Voluntario voluntario){
        super(voluntario);
        setRaio(voluntario.getRaio());
        setClassificacao(voluntario.getClassificacao());
        setNumClassificacao(voluntario.getNumClassificacao());
        aceitaMedicamentos(voluntario.aceitoTransporteMedicamentos());
        setVelocidadeMedia(voluntario.getVelocidadeMedia());
    }

    /**
     * Devolve o raio
     * @return
     */
    public double getRaio() {
        return this.raio;
    }

    /**
     * Atualiza o raio
     * @param raio
     */
    public void setRaio(double raio) {
        this.raio = raio;
    }

    /**
     * Devolve a classificacao
     * @return
     */
    public double getClassificacao() {
        return this.classificacao;
    }

    /**
     * Atualiza a classificacao
     * @param classificacao
     */
    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    /**
     * Devolve o numClassificacao
     * @return
     */
    public int getNumClassificacao() {
        return this.numClassificacao;
    }

    /**
     * Atualiza o numClassificacao
     * @param numClassificacao
     */
    public void setNumClassificacao(int numClassificacao) {
        this.numClassificacao = numClassificacao;
    }

    /**
     * Confirma se um voluntário pode transportar encomendas médicas
     * @return
     */
    public boolean aceitoTransporteMedicamentos(){
        return this.aceitaMedicas;
    }

    /**
     * Atualiza a aceitaMedicas
     * @param aceita
     */
    public void aceitaMedicamentos(boolean aceita){
        this.aceitaMedicas = aceita;
    }

    /**
     * Devolve a velocidade média
     */
    public double getVelocidadeMedia(){
        return this.velocidadeMedia;
    }

    /**
     * Atualiza a velocidade média
     */
    public void setVelocidadeMedia(double velocidadeMedia){
        this.velocidadeMedia = velocidadeMedia;
    }

    /**
     * Cria uma cópia da instância
     * @return
     */
    @Override
    public Voluntario clone() {
        return new Voluntario(this);
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;

        Voluntario voluntario = (Voluntario) o;

        return super.equals(voluntario) && this.raio == voluntario.getRaio() &&
                this.classificacao == voluntario.getClassificacao() &&
                this.numClassificacao == voluntario.getNumClassificacao() &&
                this.aceitaMedicas == voluntario. aceitoTransporteMedicamentos() &&
                this.velocidadeMedia == voluntario.getVelocidadeMedia();
    }

    /**
     * Devolve uma representação textual de Voluntario
     * @return
     */
    @Override
    public String toString() {
        return "Voluntario{" +
                super.toString() +
                ", raio= " + raio +
                ", classificacao= " + classificacao +
                ", numClassificacao= " + numClassificacao +
                ", aceito medicamentos= " + aceitaMedicas +
                ", velocidade media= " + velocidadeMedia +
                '}';
    }

    /**
     * Método que adiciona o código de uma encomenda a
     * uma coleção de encomendas
     * @param encomenda
     */
    public void addEncomenda(String encomenda){
        super.addEncomenda(encomenda);
    }


}
