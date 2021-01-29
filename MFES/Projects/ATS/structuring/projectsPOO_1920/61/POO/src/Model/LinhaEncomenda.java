package Model;

import java.io.Serializable;
import java.text.DecimalFormat;

/**
 * Classe que define uma linha de encomenda
 */
public class LinhaEncomenda implements Serializable {

    private String referencia;
    private String descricao;
    private double preco;
    private double quantidade;
    private double desconto;

    /**
     * Construtor da classe
     */
    public LinhaEncomenda() {
        this.referencia = "n/a";
        this.descricao = "n/a";
        this.preco = 0;
        this.quantidade = 0;
        this.desconto = 0;
    }

    /**
     * Construtor de classe
     *
     * @param referencia A referência do produto
     * @param descricao  A descrição do produto
     * @param preco      O preço do produto
     * @param quantidade A quantidade do produto
     * @param imposto    O imposto aplicado
     * @param desconto   O desconto aplicado
     */
    public LinhaEncomenda(String referencia, String descricao, double preco,
                          double quantidade, double imposto, double desconto) {
        this.referencia = referencia;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
        this.desconto = desconto;
    }

    /**
     * Construtor da classe
     *
     * @param linha A linha de encomenda à qual se pretende extrair a informação
     */
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.referencia = linha.getReferencia();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
        this.desconto = linha.getDesconto();
    }

    /**
     * Calcula o valor da linha de encomenda
     */
    public double calculaValorLinhaEnc() {
        double valor = this.quantidade * this.preco;
        valor -= valor * this.desconto;
        return valor;
    }

    /**
     * Calcula o desconto da linha de encomenda
     */
    public double calculaValorDesconto() {
        double valor = this.quantidade * this.preco;
        return this.calculaValorLinhaEnc() - valor;
    }

    /**
     * Indica a referência do produto
     *
     * @return A referência do produto
     */
    public String getReferencia() {
        return this.referencia;
    }

    /**
     * Define a referência do produto
     *
     * @param referencia A referência do produto a definir
     */
    public void setReferencia(String referencia) {
        this.referencia = referencia;
    }

    /**
     * Indica a descrição do produto
     *
     * @return A descrição do produto
     */
    public String getDescricao() {
        return this.descricao;
    }

    /**
     * Define descrição do produto
     *
     * @param descricao A descrição do produto a definir
     */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    /**
     * Indica o preço do produto
     *
     * @return O preço do produto
     */
    public double getPreco() {
        return this.preco;
    }

    /**
     * Define o preço do produto
     *
     * @param preco O preço do produto a definir
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Indica a quantidade
     *
     * @return A quantidade
     */
    public double getQuantidade() {
        return this.quantidade;
    }

    /**
     * Define a quantidade
     *
     * @param quantidade A quantidade a definir
     */
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * Indica o desconto
     *
     * @return O desconto
     */
    public double getDesconto() {
        return this.desconto;
    }

    /**
     * Define o desconto
     *
     * @param desconto O desconto a definir
     */
    public void setDesconto(double desconto) {
        this.desconto = desconto;
    }

    /**
     * Cria um clone da linha de encomenda
     *
     * @return O clone da linha de encomenda
     */
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * Verifica se um dado objeto é igual a uma linha de encomenda
     *
     * @param obj O objeto com o qual se pretende comparar
     * @return True caso sejam iguais e false caso contrário
     */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getReferencia().equals(this.referencia) &&
                le.getDescricao().equals(this.descricao) &&
                le.getPreco() == this.preco;
    }

    /**
     * Transforma uma linha de encomenda numa String
     *
     * @return A String correspondente à linha de encomenda
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        DecimalFormat df = new DecimalFormat("0.00");
        sb.append(String.format("%20s", descricao))
                .append(String.format("%10s", df.format(preco)))
                .append(String.format("%12s", df.format(quantidade)));

        return sb.toString();
    }
}
