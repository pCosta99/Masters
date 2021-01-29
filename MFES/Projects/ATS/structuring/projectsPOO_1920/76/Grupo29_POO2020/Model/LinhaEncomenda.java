package Model;

import java.io.Serializable;
import java.math.BigDecimal;

import View.PrintFormat;

/** Classe que representa uma linha de encomenda. */
public class LinhaEncomenda implements Serializable{
    private static final long serialVersionUID = 129L;
    /** Código do produto. */
    private String codigo;
    /** Descrição do produto. */
    private String descricao;
    /** Quantidade do produto. */
    private int    quantidade;
    /** Valor unitário do produto. */
    private BigDecimal valorUnitario;
    

    /** Construtor por omissão. */
    public LinhaEncomenda() {
        this.codigo        = "n/a";
        this.descricao     = "n/a";
        this.valorUnitario = BigDecimal.ZERO;
        this.quantidade    = 0;
    }

    /** 
     * Construtor parametrizado. 
     * @param cod       Código do produto.
     * @param desc      Descrição do produto.
     * @param quant     Quantidade do produto.
     * @param valorUnit Valor unitário do produto.
     */
    public LinhaEncomenda(String cod, String desc, int quant, BigDecimal valorUnit) {
        this.codigo        = cod;
        this.descricao     = desc;
        this.valorUnitario = valorUnit;
        this.quantidade    = quant;
    }

    /**
     * Construtor de cópia.
     * @param le Linha de encomenda.
     */
    public LinhaEncomenda(LinhaEncomenda le) {
        this.codigo        = le.getCodigo();
        this.descricao     = le.getDescricao();
        this.valorUnitario = le.getValorUnitario();
        this.quantidade    = le.getQuantidade();
    }


    /** @return Código do produto. */
    public String getCodigo() {
        return this.codigo;
    }

    /** @return Descrição do produto. */
    public String getDescricao() {
        return this.descricao;
    }

    /** @return Valor unitário do produto. */
    public BigDecimal getValorUnitario() {
        return this.valorUnitario;
    }

    /** @return Quantidade do produto. */
    public int getQuantidade() {
        return this.quantidade;
    }


    /** @param cod Código do produto. */
    public void setCodigo(String cod) {
        this.codigo = cod;
    }

    /** @param desc Descrição do produto. */
    public void setDescricao(String desc) {
        this.descricao = desc;
    }

    /** @param valor Valor unitário do produto. */
    public void setValorUnitario(BigDecimal valor) {
        this.valorUnitario = valor;
    }

    /** @param quant Quantidade do produto. */
    public void setQuantidade(int quant) {
        this.quantidade = quant;
    }


    /** 
     * Implementação do método toString().
     * @return Representação em String de uma instância da classe LinhaEncomenda.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("┉┉┉┉┉┉┫ Produto[");
        sb.append(this.codigo);
        sb.append("] ┣┉┉┉┉┉┉\n");
        sb.append("\n Descrição: ");
        sb.append(this.descricao);
        sb.append("\n Quantidade: ");
        sb.append(this.quantidade);
        sb.append("\n Valor unitário: ");
        sb.append(PrintFormat.currencyFormat(this.valorUnitario)).append("\n");

        return sb.toString();
    }

    /**
     * Implementação do método equals().
     * 
     * @param o Objeto ao qual será comparado.
     * @return Valor booleano a indicar a igualdade ao dado objeto.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;

        LinhaEncomenda prod = (LinhaEncomenda) o;
        return this.codigo.equals(prod.getCodigo())
            && this.descricao.equals(prod.getDescricao())
            && this.quantidade == prod.getQuantidade()
            && this.valorUnitario == prod.getValorUnitario();
    }

    /**
     * Implementação do método clone().
     * @return Cópia de uma dada instância de LinhaEncomenda.
     */
    @Override
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * Implementação do método hashCode().
     * @return Código de hash com base no código do produto.
     */
    @Override
    public int hashCode() {
        return codigo.hashCode();
    }
}