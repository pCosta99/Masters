package TrazAqui;

import java.io.Serializable;

/**
 * Classe que guarda as informações de um produto pedido numa encomenda.
 */
public class LinhaEncomenda implements Serializable {
    private String descricao;
    private double preco;
    private double quantidade;
    private boolean fragil;
    private String codigo;

    /**
     * Construtor vazio de uma linha de encomenda.
     */
    public LinhaEncomenda() {
        this.descricao = "";
        this.preco = 0;
        this.quantidade = 0;
        this.fragil = false;
        this.codigo = "";
    }

    /**
     * Construtor parametrizado de uma linha de encomenda.
     * @param descricao Descrição de um produto.
     * @param preco Preço de um produto.
     * @param quantidade Quantidade pedida de um produto.
     * @param fragil Boolean que indica se um produto é frágil.
     * @param codigo Código de um produto.
     */
    public LinhaEncomenda(String descricao, double preco,double quantidade,boolean fragil, String codigo) {
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade = quantidade;
        this.fragil = fragil;
        this.codigo = codigo;
    }

    /**
     * Construtor por cópia de uma linha de encomenda.
     * @param linha Linha de encomenda que pretendemos copiar.
     */
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
        this.fragil = linha.getFragil();
        this.codigo = linha.getCod();
    }

    /**
     * Getter da descrição de um produto.
     * @return Descrição do produto.
     */
    public String getDescricao() {
        return descricao;
    }

    /**
     * Setter da descrição de um produto.
     * @param descricao Descrição do produto.
     */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    /**
     * Getter do preço de um produto.
     * @return Preço do produto.
     */
    public double getPreco() {
        return preco;
    }

    /**
     * Setter do preço de um produto.
     * @param preco Preço de um produto.
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Getter da quantidade de um produto.
     * @return Quantidade de um produto.
     */
    public double getQuantidade() {
        return quantidade;
    }

    /**
     * Setter da quantidade de um produto.
     * @param quantidade Quantidade de um produto.
     */
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * Getter da fragilidade de um produto.
     * @return Fragilidade de um produto.
     */
    public boolean getFragil() {
        return fragil;
    }

    /**
     * Getter do código de um produto.
     * @return Código de um produto.
     */
    public String getCod() {
        return codigo;
    }

    /**
     * Setter de um código de um produto.
     * @param codigo Código de um produto.
     */
    public void setCod(String codigo) {
        this.codigo = codigo;
    }

    /**
     * Método que clona uma linha de encomenda.
     * @return Linha de uma encomenda clonada.
     */
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * Método que compara duas linhas de encomenda.
     * @param obj Linha de encomenda que vamos comparar.
     * @return Booleano que indica se são iguais ou não.
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return  le.getDescricao().equals(this.descricao) &&
                le.getPreco() == this.preco &&
                le.getCod().equals(this.codigo) &&
                le.getFragil()==this.fragil &&
                le.getQuantidade()==this.quantidade;
    }

    /**
     * Método que converte uma linha de encomenda para uma string.
     * @return Linha de encomenda convertida numa string.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("LinhaEncomenda{");
        sb.append("descricao='").append(descricao).append('\'');
        sb.append(", preco=").append(preco);
        sb.append(", quantidade=").append(quantidade);
        sb.append(", fragil=").append(fragil);
        sb.append(", codigo='").append(codigo).append('\'');
        sb.append('}');
        return sb.toString();
    }


}
