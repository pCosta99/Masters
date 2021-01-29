package Model;

import java.util.Set;
import java.util.TreeSet;

public class EmpresaTransporte extends AtorSistema implements java.io.Serializable{

    private double raio;
    private double precoKm;
    private double classificacao;
    private int numClassificacao;
    private boolean aceitaMedicas;
    private double velocidadeMedia;
    private double precoPorKg;

    /**
     * Construtor vazio
     */
    public EmpresaTransporte(){
        super();
        this.raio = 0;
        this.precoKm = 0;
        this.classificacao = 0;
        this.numClassificacao = 0;
        this.aceitaMedicas = false;
        this.velocidadeMedia = 0;
        this.precoPorKg = 0;
    }

    /**
     * Construtor parametrizado
     * @param cod
     * @param email
     * @param nif
     * @param nome
     * @param password
     * @param localizacao
     * @param raio
     * @param precoKm
     * @param classificacao
     * @param numClassificacao
     */
    public EmpresaTransporte(String cod, String email, String nif, String nome, String password,
                             Coordenadas localizacao, double raio, double precoKm, double classificacao,
                             int numClassificacao, boolean aceita, double velocidade, double precoKg){
        super(cod,email,nif,nome,password,localizacao);
        setRaio(raio);
        setPrecoKm(precoKm);
        setClassificacao(classificacao);
        setNumClassificacao(numClassificacao);
        aceitaMedicas(aceita);
        setVelocidadeMedia(velocidade);
        setPrecoKg(precoKg);
    }

    /**
     * Construtor por cópia
     * @param empresaTransporte
     */
    public EmpresaTransporte(EmpresaTransporte empresaTransporte){
        super(empresaTransporte);
        setRaio(empresaTransporte.getRaio());
        setPrecoKm(empresaTransporte.getPrecoKm());
        setEncomendas(empresaTransporte.getEncomendas());
        setClassificacao(empresaTransporte.getClassificacao());
        setNumClassificacao(empresaTransporte.getNumClassificacao());
        aceitaMedicas(empresaTransporte.aceitoTransporteMedicamentos());
        setVelocidadeMedia(empresaTransporte.getVelocidadeMedia());
        setPrecoKg(empresaTransporte.getPrecoKg());
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
     * Devolve o precoKm
     * @return
     */
    public double getPrecoKm() {
        return this.precoKm;
    }

    /**
     * Atualiza o precoKm
     * @param precoKm
     */
    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }

    /**
     * Devolve a classificacao
     * @return
     */
    public double getClassificacao(){
        return this.classificacao;
    }

    /**
     * Atualiza a classificacao
     * @param novaClassificacao
     */
    public void setClassificacao(double novaClassificacao){
        this.classificacao = novaClassificacao;
    }

    /**
     * Devolve o número de classificacoes
     * @return
     */
    public int getNumClassificacao(){
        return this.numClassificacao;
    }

    /**
     * Atualiza o número de classificacoes
     * @param novoNumClassificacoes
     */
    public void setNumClassificacao(int novoNumClassificacoes){
        this.classificacao = novoNumClassificacoes;
    }

    /**
     * Confirma se uma empresa pode transportar encomendas médicas ou não
     * @return
     */
    public boolean aceitoTransporteMedicamentos(){
        return this.aceitaMedicas;
    }

    /**
     * Atualiza o aceitaMedicas
     * @param aceita
     */
    public void aceitaMedicas(boolean aceita){
        this.aceitaMedicas = aceita;
    }

    /**
     * Devolve a velocidade média
     * @return
     */
    public double getVelocidadeMedia(){
        return this.velocidadeMedia;
    }

    /**
     * Atualiza a velocidade média
     * @param novaVelocidade
     */
    public void setVelocidadeMedia(double novaVelocidade){
        this.velocidadeMedia = novaVelocidade;
    }

    /**
     * Devolve o preço por Kg
     * @return
     */
    public double getPrecoKg(){
        return this.precoPorKg;
    }

    /**
     * Atualiza o preço por Kg
     * @param preco
     */
    public void setPrecoKg(double preco){

    }

    /**
     * Devolve uma cópia da instância
     * @return
     */
    @Override
    public EmpresaTransporte clone() {
        return new EmpresaTransporte(this);
    }

    /**
     * Devolve o custo de uma encomenda
     * @param cordLoja
     * @param cordUtilizador
     * @return
     */
    public double custoEncomendas(Coordenadas cordLoja, Coordenadas cordUtilizador, Double peso){
        double distancia =  super.getLocalizacao().distanciaKm(cordLoja) + super.getLocalizacao().distanciaKm(cordUtilizador);
        double precoPeso = peso * precoPorKg;

        return distancia * this.precoKm + precoPeso;
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

        EmpresaTransporte empresa = (EmpresaTransporte) o;

        return super.equals(empresa) &&
                this.raio == empresa.getRaio() &&
                this.precoKm == empresa.getPrecoKm() &&
                this.classificacao == empresa.getClassificacao() &&
                this.numClassificacao == empresa.getNumClassificacao() &&
                this.aceitaMedicas == empresa.aceitoTransporteMedicamentos() &&
                this.velocidadeMedia == empresa.getVelocidadeMedia() &&
                this.precoPorKg == empresa.getPrecoKg();

    }

    /**
     * Devolve uma representação textual
     * @return
     */
    @Override
    public String toString() {
        return "EmpresaTransporte{" +
                ", raio=" + raio +
                ", precoKm=" + precoKm +
                ", classifação=" + classificacao +
                ", número classificações=" + numClassificacao +
                ", aceita encomendas médicas= " + aceitaMedicas +
                ", velocidade média=" + velocidadeMedia +
                ", preço por Kg= " + precoPorKg +
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
