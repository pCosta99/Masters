import java.io.Serializable;

public class LinhaEnc implements Serializable
{
    /** identificador de produto. */
    private String codProduto;

    /** descrição de produto */
    private String descricao;

    /** quantidade de produto */
    private double quantidade;

    /** preço unitário de produto */
    private double valorUni;

    /** peso unitário de produto */
    private double pesoUni;

    /**  verificação se o produto é de teor médico.
     * (true-produto médico,false-produto não médico */
    private boolean produtoMedico;


    /**
     * Construtor por omissão de Linha de Encomenda
     */
    LinhaEnc(){
        this.codProduto = new String();
        this.descricao = new String();
        this.quantidade = 0;
        this.valorUni = 0;
        this.pesoUni = 0;
        this.produtoMedico = false;
    }


    /**
     * Construtor parametrizado de Linha de Encomenda
     * @param codProduto codigo de produto
     * @param descricao descrição de produto
     * @param quantidade quantidade de produto
     * @param valorUni preço unitário de produto
     * @param pesoUni peso unitário de produto
     * @param prodMed verifica se é produto médico
     */
    LinhaEnc(String codProduto,String descricao,double quantidade,double valorUni,double pesoUni,boolean prodMed){
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUni = valorUni;
        this.pesoUni = pesoUni;
        this.produtoMedico = prodMed;
    }

    /**
     * Construtor por cópia de Linha de Encomenda
     * @param e LinhaEnc
     */
    LinhaEnc(LinhaEnc e){
        setCodProduto(e.getCodProduto());
        setDescricao(e.getDescricao());
        setQuantidade(e.getQuantidade());
        setValorUni(e.getValorUni());
        setPesoUni(e.getPesoUni());
        setProdMed(e.getProdMed());
    }

    /**
     * Getter do identificador do produto
     * @return identificador do produto
     */
    public String getCodProduto(){return this.codProduto;}

    /**
     * Getter da descrição do produto
     * @return descrição do produto
     */
    public String getDescricao(){return this.descricao;}

    /**
     * Getter da quantidade de produto
     * @return quantidade de produto
     */
    public double getQuantidade(){return this.quantidade;}

    /**
     * Getter do preço unitário de produto
     * @return preço unitário de produto
     */
    public double getValorUni(){return this.valorUni;}

    /**
     * Getter do peso de produto
     * @return peso de produto
     */
    public double getPesoUni(){return this.pesoUni;}

    /**
     * Getter da verificação se é produto médico
     * @return verificação se é produto médico
     */
    public boolean getProdMed(){return this.produtoMedico;}

    /**
     * Setter do identificador do produto.
     * @param novo identificador do produto
     */
    public void setCodProduto(String novo){this.codProduto = novo;}

    /**
     * Setter da descrição do produto.
     * @param novo descrição do produto.
     */
    public void setDescricao(String novo){this.descricao = novo;}

    /**
     * Setter da quantidade de produto.
     * @param novo quantidade de produto.
     */
    public void setQuantidade(double novo){this.quantidade = novo;}

    /**
     * Setter da preço unitário de produto.
     * @param novo preço unitário de produto.
     */
    public void setValorUni(double novo){this.valorUni = novo;}

    /**
     * Setter da peso de produto.
     * @param novo peso de produto.
     */
    public void setPesoUni(double novo){this.pesoUni = novo;}

    /**
     * Setter da verificação se é produto médico.
     * @param novo verificação se é produto médico.
     */
    public void setProdMed(boolean novo){this.produtoMedico = novo;}


    /**
     * Metodo clone de uma Linha de encomenda/produto
     * @return clone da Linha de encomenda/produto
     */
    public LinhaEnc clone(){return new LinhaEnc(this);}

    /**
     * Metodo equals de uma Linha de encomenda/produto
     * @param o Objeto
     * @return verificação se é Linha de encomenda igual(booleano)
     */
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass()!=this.getClass()) return false;
        LinhaEnc e = (LinhaEnc) o;
        return this.codProduto.equals(e.getCodProduto()) &&
                this.descricao.equals(e.getDescricao()) &&
                this.quantidade == e.getQuantidade() &&
                this.valorUni == e.getValorUni() &&
                this.pesoUni == e.getPesoUni() &&
                this.produtoMedico == e.getProdMed();
    }

    /**
     * Metodo toString de uma Linha de encomenda
     * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append(this.codProduto).append(",")
                .append(this.descricao).append(",")
                .append(this.quantidade).append(",")
                .append(this.valorUni).append(",")
                .append(this.pesoUni).append("\n");
        return s.toString();
    }


    /**
     * Metodo que devolve uma String com o codigo, descrição e preço de um produto
     * @return String
     */
    public String imprime(){
        StringBuilder s = new StringBuilder();
        s.append("Codigo do produto: ").append(this.codProduto).append(" , ")
                .append("Descriçao: ").append(this.descricao).append(" , ")
                .append("Preço: ").append(this.valorUni).append("\n");
        return s.toString();
    }


    /**
     * Metodo que calcula o valor de uma linha de encomenda.
     * @return double
     */
    public double calculaValorLinha(){return this.quantidade * this.valorUni;}
}