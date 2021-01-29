package Projeto.Entidades;
import Projeto.Exceptions.IdRepetidoException;
import Projeto.Interfaces.IEncomenda;
import Projeto.Interfaces.ILoja;
import Projeto.Interfaces.IProduto;
import Projeto.Util.GPS;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * A classe loja e uma das heranças da classe Utilizador e tem como objetivo controlar os metodos relativos as lojas.
 * Esta classe tem como variaveis de instancia (para alem dos que foram herdados da Utilizador): 
 * um booleano para verificar se a loja oferece dados tais como o tempo medio de espera nas filas,
 * caso forneça esses dados, tera acesso ao tamanho da fila e ao tempo de atendimento medio
 */
public class Loja extends Entidade implements ILoja, Serializable {
    private boolean dadosFila;
    private int sizeFila;
    private float tempAtendMed;
    private Collection<IProduto> listProds;

    /*
     * Construtores da Classe Encomenda.
     * Declaracao dos construtores por omissao, parametrizado e de copia.
     */
    /**
     * Construtor por omissao de Loja
     */
    public Loja() {
        super();
        this.dadosFila = false;
        this.sizeFila = 0;
        this.tempAtendMed = 0;
        this.listProds = new ArrayList<>();
    }
    
    /**
     * Construtor parametrizado de Loja
     */
    public Loja(String id, String pw, String nome, GPS loc, Collection<IEncomenda> encs, boolean dadosFila, int tamanhoFila,
                float tempAtendMed, Collection<IProduto> listProds) {
        super(id, pw, nome, loc, encs);
        this.setDFila(dadosFila);
        this.setSize(tamanhoFila);
        this.setTempMed(tempAtendMed);
        this.setListProds(listProds);
    }
    
    /**
     * Construtor por copia de Loja.
     * Aceita como parametro outra Loja e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Loja(Loja l) {
        super(l);
        this.dadosFila = l.getDFila();
        this.sizeFila = l.getSize();
        this.tempAtendMed = l.getTempMed();
        this.listProds = l.getListProds();
    }

    /*
     * Getters e Setters
     */
    /**
     * Metodo que retorna a decisao de aceitar fornecer os dados da fila.
     */
    public boolean getDFila() {
        return this.dadosFila;
    }
    
    /**
     * Metodo que retorna o tamanho da fila de espera.
     */
    public int getSize() {
        return this.sizeFila;
    }
    
    /**
     * Metodo que retorna o tempo medio de atendimento de uma dada loja.
     */
    public float getTempMed() {
        return this.tempAtendMed;
    }

    /**
     * Metodo que retorna a lista de Produtos de uma dada loja.
     */
    public Collection<IProduto> getListProds() {
        return this.listProds.stream()
                .map(IProduto::clone)
                .collect(Collectors.toList());
    }

    /**
     * Metodo que atualiza a decisao de fornecer dados sobre a fila de espera.
     */
    public void setDFila(boolean dados) {
        this.dadosFila = dados;
    }
    
    /**
     * Metodo que atualiza o tamanho da fila de espera.
     * @param size - novo tamanho da fila
     */
    public void setSize(int size) {
        this.sizeFila = size;
    }
    
    /**
     * Metodo que atualiza o tempo medio de atendimento de uma dada loja.
     * @param tempo - novo tempo de atendimento medio
     */
    public void setTempMed(float tempo) {
        this.tempAtendMed = tempo;
    }

    /**
     * Metodo que atualiza a lista de Produtos de uma dada loja.
     * @param listProds - nova lista de produtos
     */
    public void setListProds(Collection<IProduto> listProds) {
        this.listProds = listProds.stream()
                .map(IProduto::clone)
                .collect(Collectors.toList());
    }

    /*
     * Restantes Metodos de Instancia
     */
    /**
     * Metodo que calcula o tempo que demora a atender todas as pessoas na fila, ?????????????????????'
     * com base no numero de pessoas que estao de momento na fila de espera e no tempo de atendimento medio por pessoa.
     */
    public int calculaTempoFila() {
        return Math.round(this.sizeFila*this.tempAtendMed);
    }

    /**
     * Metodo que adiciona um Produto à lista de produtos.
     */
    public void addProduto(IProduto p) throws IdRepetidoException {
        if(!this.listProds.contains(p)) {
            this.listProds.add(p.clone());
        } else throw new IdRepetidoException("Já existe um produto com esse id");
    }

    /**
     * Metodo que remove um Produto da lista de produtos.
     */
    public void removeProduto(String codigo) {
        this.listProds.removeIf(prod -> prod.getCodigo().equals(codigo));
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Loja l = (Loja) o;
        return super.equals(l);
    }

    /**
     * Converte a classe Loja para String
     */

    public String toString() {
        return super.toString() +
                "Aceita dados da fila?: " + this.dadosFila +
                "\nTamanho da fila: " + this.sizeFila +
                "\nTempo médio de atendimento: " + this.tempAtendMed;
    }
    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public Loja clone() {
        return (this);
    }

    /**
     * Método hashCode
     */
    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), dadosFila, sizeFila, tempAtendMed, listProds);
    }
}
