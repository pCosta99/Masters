package src.model;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * Classe que cria e regista Entregas
 */

public class Entrega implements Comparable<Entrega>, Serializable {

    //Variáveis de Instância

    private double custo;
    private LocalDateTime dataLevantamento;
    private LocalDateTime dataFinal;
    private Encomenda encomenda;
    private String codDistribuidor;

    /**
     * Construtores da classe model.Entrega.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Entrega.
     */

    public Entrega(){
        this.custo = 0;
        this.dataLevantamento = LocalDateTime.now();
        this.dataFinal = LocalDateTime.now();
        this.encomenda = new Encomenda();
        this.codDistribuidor = "";
    }

    /**
     * Construtor parametrizado de model.Entrega.
     * Aceita como parâmetros os valores para cada variável de instância.
     */

    public Entrega(double custo, LocalDateTime dataLevantamento, LocalDateTime dataFinal, Encomenda e, String codDistribuidor){
        this.custo = custo;
        this.dataLevantamento = dataLevantamento;
        this.dataFinal = dataFinal;
        this.encomenda = e.clone();
        this.codDistribuidor = codDistribuidor;
    }

    /**
     * Construtor de cópia de model.Entrega.
     * Aceita como parâmetro outra model.Entrega e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Entrega(Entrega e){
        this.custo = e.getCusto();
        this.dataLevantamento = e.getDataLevantamento();
        this.dataFinal = e.getDataFinal();
        this.encomenda = e.getEncomenda();
        this.codDistribuidor = e.getCodDistribuidor();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o custo da model.Entrega
     * @return double custo da model.Entrega
     */

    public double getCusto() {
        return this.custo;
    }

    /**
     * Devolve a data em que a model.Entrega foi feita
     * @return LocalDate Data Final da model.Entrega
     */

    public LocalDateTime getDataFinal() {
        return this.dataFinal;
    }

    /**
     * Devolve a data em que a model.Entrega foi pedida
     * @return LocalDate Data em que a model.Entrega foi pedida
     */

    public LocalDateTime getDataLevantamento() {
        return this.dataLevantamento;
    }

    /**
     * Devolve a model.Encomenda da model.Entrega
     * @return model.Encomenda cópia da model.Encomenda presente na model.Entrega
     */

    public Encomenda getEncomenda() {
        return this.encomenda.clone();
    }

    /**
     * Devolve o código do model.Distribuidor associado á entrega
     * @return String condendo o código do model.Distribuidor
     */
    public String getCodDistribuidor(){
        return this.codDistribuidor;
    }

    /**
     * Atualiza o custo da model.Entrega
     * @param custo novo custo da model.Entrega
     */

    public void setCusto(double custo) {
        this.custo = custo;
    }

    /**
     * Atualiza a data em que a model.Entrega foi feita
     * @param dataFinal nova data em que a model.Entrega foi feita
     */

    public void setDataFinal(LocalDateTime dataFinal) {
        this.dataFinal = dataFinal;
    }

    /**
     * Atualiza a data em que a model.Entrega foi pedida
     * @param dataLevantamento nova data em que a model.Entrega foi pedida
     */

    public void setDataLevantamento(LocalDateTime dataLevantamento) {
        this.dataLevantamento = dataLevantamento;
    }

    /**
     * Atualiza a encomenda presente na model.Entrega
     * @param encomenda nova encomenda da model.Entrega
     */

    public void setEncomenda(Encomenda encomenda) {
        this.encomenda = encomenda.clone();
    }

    /**
     *
     */

    public void setCodDistribuidor(String codDistribuidor) {
        this.codDistribuidor = codDistribuidor;
    }

    /**
     * Método que transforma uma model.Entrega numa String
     * @return String com toda a informação da model.Entrega
     */

    public String toString() {
        final StringBuilder sb = new StringBuilder("model.Entrega{");
        sb.append("custo=").append(this.custo);
        sb.append("codDistribuidor=").append(this.codDistribuidor);
        sb.append(", dataLevantamento=").append(this.dataLevantamento.toString());
        sb.append(", dataFinal=").append(this.dataFinal.toString());
        sb.append(", encomenda=").append(this.encomenda.toString());
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método que determina se uma model.Entrega e um Objeto são iguais
     * @param o Objeto qualquer
     * @return true caso a model.Entrega e o Objeto sejam iguais, e vice versa
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Entrega entrega = (Entrega) o;
        return entrega.getCusto() == this.custo &&
                this.dataLevantamento.equals(entrega.getDataLevantamento()) &&
                this.dataFinal.equals(entrega.getDataFinal()) &&
                this.encomenda.equals(entrega.getEncomenda()) &&
                this.codDistribuidor.equals(entrega.getCodDistribuidor());
    }

    /**
     * Método que clona uma model.Entrega, para tal é usado o construtor de cópia
     * @return Objeto model.Entrega que é uma cópia da model.Entrega
     */

    public Entrega clone(){
        return new Entrega(this);
    }


    /**
     * Método que compara o this model.Distribuidor com outro model.Distribuidor a partir da ordem lexicográfica de seus usernames
     * @param e2 model.Entrega com o qual o this será comparado
     * @return inteiro correspondente à comparação da ordem lexicográfica associada
     */
    public int compareTo(Entrega e2){
        return this.dataLevantamento.compareTo(e2.getDataLevantamento());
    }

}
