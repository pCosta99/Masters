package src.model;

import java.io.Serializable;

/**
 * Classe que cria Produtos
 */

public class Produto implements Serializable {

    //Variáveis de classe    
    private static String codProdGlobal = "p0";

    //Variáveis de Instância
    private String codProd;
    private String nome;
    private double custo;
    private double peso;
    private boolean medicamento;



    /**
     * Métodos de Classe
     */

    /**
     * Devolve o código de produto gerado para o próximo produto a a ser criado
     * @return String com o novo código de produto
     */
    private static String getCodProdGlobal(){
        String cod = Produto.codProdGlobal;
        String inc = cod.substring(1);
        int i = Integer.parseInt(inc);
        i = i+1;
        Produto.setCodProdGlobal("p"+i);
        return cod;
    }

    /**
     * Atualiza o código de produto gerado para o próximo produto a ser criado
     * @param cod String com o novo código de produto que substituirá o atual
     */
    private static void setCodProdGlobal(String cod){
        Produto.codProdGlobal = cod;
    }

    /**
    * Atualiza o código de produto, substuituindo o antigo
    * @param cod String o codigo atualizado
    */

    public static void updateCodGlobal(String cod){
        String atual = Produto.getCodProdGlobal();
        int numberAtual = Integer.parseInt(atual.substring(1));
        int numberCod = Integer.parseInt(cod.substring(1));
        if (numberCod > numberAtual) {
            Produto.setCodProdGlobal("p"+(numberCod+1));
        }
    }

    /**
     * Construtores da classe model.Produto.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Produto.
     */

    public Produto(){
        this.codProd = Produto.getCodProdGlobal();
        this.nome = "";
        this.custo = 0;
        this.peso = 0;
        this.medicamento = false;
    }

    /**
     * Construtor parametrizado de model.Produto.
     * Aceita como parâmetros os valores para cada variável de instância.
     */

    public Produto(String nome, double custo, double peso, boolean medicamento){
        this.codProd = Produto.getCodProdGlobal();
        this.nome = nome;
        this.custo = custo;
        this.peso = peso;
        this.medicamento = medicamento;
    }

    /**
     * Construtor de cópia de model.Produto.
     * Aceita como parâmetro outro model.Produto e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Produto(Produto p){
        this.codProd = p.getCodProd();
        this.nome = p.getNome();
        this.custo = p.getCusto();
        this.peso = p.getPeso();
        this.medicamento = p.getMedicamento();
    }
    
    /**
    * Construtor parametrizado de model.Produto
    * Aceita como pârametros atualizados para cada variável de instância
    */    



    public Produto(String cod, String nome, double custo, double peso, boolean medicamento){
        this.codProd = cod;
        this.nome = nome;
        this.custo = custo;
        this.peso = peso;
        this.medicamento = medicamento;
        Produto.updateCodGlobal(cod);
    }


    /**
     * Métodos de Instância
     */

    /**
     * Devolve o nome do model.Produto
     * @return nome do model.Produto
     */

    public String getNome() {
        return this.nome;
    }

    /**
     * Devolve o custo de uma unidade do produto
     * @return custo do model.Produto
     */

    public double getCusto() {
        return this.custo;
    }

    /**
     * Devolve o peso do model.Produto
     * @return peso do model.Produto
     */

    public double getPeso() {
        return this.peso;
    }

    /**
     * Devolve o código do model.Produto
     * @return código do model.Produto
     */

    public String getCodProd() {
        return this.codProd;
    }

    /**
     * Devolve um boolean que determina se o model.Produto é um medicamento
     * @return true caso seja um medicamento, false caso não seja
     */

    public boolean getMedicamento() {
        return this.medicamento;
    }

    /**
     * Atualiza o código do model.Produto
     * @param codProd novo código do model.Produto
     */

    public void setCodProd(String codProd) {
        this.codProd = codProd;
    }

    /**
     * Atualiza o nome do model.Produto
     * @param nome novo nome do model.Produto
     */

    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Atualiza o custo do model.Produto
     * @param custo novo custo do model.Produto
     */

    public void setCusto(double custo) {
        this.custo = custo;
    }

    /**
     * Atualiza o estado de ser ou não medicamento
     * @param medicamento novo estado
     */

    public void setMedicamento(boolean medicamento) {
        this.medicamento = medicamento;
    }

    /**
     * Atualiza o peso do model.Produto
     * @param peso novo peso do model.Produto
     */

    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Método que determina se o model.Produto é igual a um outro Object
     * @param o Objeto a comparar
     * @return true caso sejam iguais, false caso não sejam
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return produto.getCusto() == this.custo &&
                produto.getPeso() == this.peso  &&
                this.medicamento == produto.getMedicamento() &&
                this.codProd.equals( produto.getCodProd()) &&
                this.nome.equals( produto.getNome());
    }

    /**
     * Método que transforma um model.Produto numa String
     * @return String com toda a informação do model.Produto
     */


    public String toString() {
        final StringBuilder sb = new StringBuilder("( ");
        sb.append("codProd= '").append(this.codProd).append('\'');
        sb.append(", nome= '").append(this.nome).append('\'');
        sb.append(", custo= ").append(this.custo);
        sb.append(", peso= ").append(this.peso);
        sb.append(", medicamento=").append(this.medicamento);
        sb.append(')');
        return sb.toString();
    }

    /**
     * Método que clona um model.Produto
     * Para tal invoca o construtor de cópia
     * @return cópia de model.Produto
     */

    public Produto clone(){
        return new Produto(this);
    }


}
