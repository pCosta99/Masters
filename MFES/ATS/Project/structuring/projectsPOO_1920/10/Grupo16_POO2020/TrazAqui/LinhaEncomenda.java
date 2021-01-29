package TrazAqui;

import java.io.Serializable;
/**
 * Classe que representa uma LinhaEncomenda TrazAqui!
 */
public class LinhaEncomenda implements Serializable {
    /**
     *
     */
    private String referencia;
    /**
     * Referência da linha de encomenda
     */
    private String descricao;
    /**
     * Descrição do produto
     */
    private double preco;
    /**
     * Preço total de uma linha de Encomenda
     */
    private double quantidade;
    /**
     * Valor unitário de um produto
     */
    private double valorUni;

    /**
     * Construtor vazio de um objeto LinhaEcomenda
     */
    public LinhaEncomenda() {
        this.referencia = "n/a";
        this.descricao = "n/a";
        this.preco = 0;
        this.quantidade = 0;
        valorUni = 0;
    }
    /**
     * Construtor parametrizado de um objeto LinhaEncomenda
     * @param referencia String que representa a referencia de um objeto LinhaEncomenda
     * @param descricao String que indica a descricão de um objeto LinhaEncomenda
     * @param preco double que india o preço do todos os produtos que existem
     * @param quantidade double que indica a quantidade de produtos que existem
     * @param v double que indica quanto custa cada produto (preco/quantidade)
     */
    public LinhaEncomenda(String referencia, String descricao, double preco,
                double quantidade, double v) {
        this.referencia = referencia;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
        this.valorUni = v;
    }
    /**
     * Construtor copia de um objeto LinhaEncomenda
     * @param linha LinhaEncomenda
     */
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.referencia = linha.getReferencia();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
        this.valorUni = linha.getValorUni();
    }

    /**
     * Método que vai calcular quanta vale cada LinhaEncomenda
     * @return double quanto vale cada linhaEncomenda
     */
    public double calculaValorLinhaEnc() {
        return this.quantidade * this.valorUni;
    }
    /**
     * Método para obter a Referencia
     * @return String referencia
     */
    public String getReferencia() {
        return this.referencia;
    }
    /**
     * Método para dar set da Referencia
     * @param referencia String
     * */
     public void setReferencia(String referencia) {
        this.referencia = referencia;
    }
    /**
     * Método para obter a Descricao
     * @return String descricao
     */
    public String getDescricao() {
        return this.descricao;
    }
    /**
     * Método para dar set da Descricao
     * @param descricao String
     * */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }
    /**
     * Método para obter o preco
     * @return double preco
     */
    public double getPreco() {
        return this.preco;
    }
    /**
     * Método para dar set do preco
     * @param preco float
     * */
    public void setPreco(double preco) {
        this.preco = preco;
    }
    /**
     * Método para obter a Quantidade
     * @return double quantidade
     */
    public double getQuantidade() {
        return this.quantidade;
    }
    /**
     * Método para dar set da quantidade
     * @param quantidade double
     * */
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }
    /**
     * Método para obter o valor de cada unidade
     * @return double valorUni
     */
    public double getValorUni() {
        return valorUni;
    }
    /**
     * Método para dar set do valor de cada unidade
     * @param valorUni double
     * */
    public void setValorUni(double valorUni) {
        this.valorUni = valorUni;
    }

    /**
     * Método clone
     */
    @Override
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * Método para verificar se um certo objeto é igual a LinhaEncomenda
     * @param obj Objeto a comparar
     * @return true caso seja igual false caso contrário
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getReferencia().equals(this.referencia) &&
              le.getDescricao().equals(this.descricao) &&
              le.getPreco() == this.preco;
    }

    /**
     * Método que cria uma String de um objeto em concreto (Neste caso de LinhaEncomenda)
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder("   >>LinhaEncomenda: ");
        sb.append(referencia).append(' ');
        sb.append(", DESCRIÇÃO: ").append(descricao);
        sb.append(", PREÇO: ").append(preco);
        sb.append(", QUANTIDADE: ").append(quantidade);
        sb.append(", VALOR UNITÁRIO: ").append(valorUni).append("\n");
        return sb.toString();
    }
}
