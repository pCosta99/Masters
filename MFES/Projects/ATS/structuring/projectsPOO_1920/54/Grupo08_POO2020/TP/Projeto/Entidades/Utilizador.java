package Projeto.Entidades;
import Projeto.Interfaces.IUtilizador;
import Projeto.Interfaces.IEncomenda;
import Projeto.Util.GPS;

import java.io.Serializable;
import java.util.Collection;
import java.util.Objects;

/**
 * Classe que implementa um cliente.
 * A classe Cliente e uma das heranças da classe Utilizador e tem como objetivo controlar os metodos relativos aos clientes.
 * Esta classe tem como variaveis de instancia (para alem das que foram herdadas da Utilizador):
 * um nif caso o cliente pretenda receber uma fatura da encomenda que solicitou.
 */
public class Utilizador extends Entidade implements IUtilizador, Serializable {
    private String nif;
    
     /**
     * Construtor por omissao de Cliente
     */
    public Utilizador() {
        super();
        this.nif = "";
    }    
    
    /**
     * Construtor parametrizado de Cliente.
     * Aceita como parametros uma String com o id do cliente, uma String com o nome do cliente, um GPS com a localizaçao do cliente,
     * uma Collection com as encomendas e um String com o nif do cliente.????????????????????
     */
    public Utilizador(String id, String pw, String nome, GPS loc, Collection<IEncomenda> encs, String n) {
        super(id, pw, nome, loc, encs);
        this.nif = n;
    }
     
    /**
     * Construtor por copia de Cliente
     * Aceita como parametro outro Cliente e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Utilizador(Utilizador c) {
        super(c);
        this.nif = getNIF();
    }
    
    /**
     * Retorna o numero de contribuinte do cliente
     */
    public String getNIF() {
        return this.nif;
    }
    
    /**
     * Atualiza o valor do numero de contribuinte
     * @param novoNIF String do novo cliente
     */
        public void setNIF(String novoNIF) {
        this.nif = novoNIF;
    }

    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public Utilizador clone() {
        return new Utilizador(this);
    }
    
    /**
     * Metodo que determina se dois Clientes são iguais.
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Utilizador l = (Utilizador) o;
        return this.nif.equals(l.getNIF()); 
    }

    /**
     * Metodo que devolve a representaçao em String do Cliente.
     * @return String com as variaveis desta instancia.
     */
    @Override
    public String toString() {
        return super.toString() +
                "\nNif: " + this.nif;
    }

    /**
     * Verifica se o cliente comprou através de uma determinada empresa de transportes
     * @param idEmpresa - ID da empresa
     */
    public boolean clienteComprouEmpresa(String idEmpresa) {
        for(IEncomenda encomenda : this.getEncomendas())
            if (encomenda.getIdTransportador().equals(idEmpresa))
                return true;
        return false;
    }

    /**
     * Método hashCode
     */
    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), nif);
    }
}