package Projeto.Entidades;

import Projeto.Interfaces.IProduto;
import java.io.Serializable;
import java.util.Objects;

/**
 * Classe que implementa o Produto
 * Um Produto é caracterizado pelo seu nome, código, peso,
 * preço unitário e um boolean para verificar se é um produto medicinal
 */
public class Produto implements IProduto, Serializable {
    private String nome;
    private String codigo;
    private double peso;
    private float preco;
    private boolean medicinal;

    /**
     * Construtor por omissão
     */
    public Produto() {
        this.nome = "";
        this.codigo = "";
        this.peso = 0;
        this.preco = 0;
        this.medicinal = false;
    }

    /**
     * Construtor parametrizsdo
     */
    public Produto(String nome, String codigo, double peso, float preco, boolean medicinal) {
        this.nome = nome;
        this.codigo = codigo;
        this.peso = peso;
        this.preco = preco;
        this.medicinal = medicinal;
    }

    /**
     * Construtor por copia de Produto.
     * Aceita como parametro outro Produto e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Produto(Produto p) {
        this.nome = p.getNome();
        this.codigo = p.getCodigo();
        this.peso = p.getPeso();
        this.preco = p.getPreco();
        this.medicinal = p.getMedicinal();
    }
    /*
     * Getters e Setters
     */
    /**
     * Metodo que devolve o nome do Produto.
     */
    public String getNome() {
        return nome;
    }

    /**
     * Metodo que atualiza o nome do Produto.
     * @param nome - novo nome do produto
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     *  Metodo que devolve o peso do Produto.
     */
    public String getCodigo() {
        return codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public double getPeso() {
        return peso;
    }

    /**
     * Metodo que atualiza o peso do produto.
     * @param peso - novo peso do produto
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Metodo que devolve o preço do Produto.
     */
    public float getPreco() {
        return preco;
    }

    /**
     * Metodo que atualiza o preco do Produto.
     * @param preco - novo preco do produto
     */
    public void setPreco(float preco) {
        this.preco = preco;
    }

    public boolean getMedicinal() {
        return medicinal;
    }

    public void setMedicinal(boolean medicinal) {
        this.medicinal = medicinal;
    }

    /*
     * Métodos equals, toString e clone
     */
    /**
     * Metodo que determina se dois Prodtuos sao iguais.
     * @return boolean verdadeiro caso o preco, peso e nome de dois Produtos seja igual.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return Double.compare(produto.peso, peso) == 0 &&
                Float.compare(produto.preco, preco) == 0 &&
                Objects.equals(nome, produto.nome) &&
                Objects.equals(codigo, produto.codigo) &&
                this.medicinal == produto.getMedicinal();
    }

    /**
     * Metodo que devolve a representaçao em String da Produto.
     * @return String com as variaveis desta instancia.
     */
    @Override
    public String toString() {
        return '\'' + this.nome + '\'' +
                "\n\tPeso = " + this.peso +
                "\n\tPreco = " + this.preco;
    }

    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public Produto clone() {
        return new Produto(this);
    }

    /**
     * Método hashCode
     */
    @Override
    public int hashCode() {
        return Objects.hash(nome, codigo, peso, preco, medicinal);
    }
}