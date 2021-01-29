package Projeto.Util;

import java.io.Serializable;

/**
 * Classe que implenta um estado.
 * Um estado serve para averiguar a disponibilidade de um Voluntario ou de uma Empresa.
 * Será uma string que nos diz qual a disponibilidade: pode estar "Ocupado" ou "Livre".
 */
public class Estado implements Serializable {
    private String estado;
    
    /*
     * Construtores da classe Estado.
     * Declaracao dos construtores por omissao, parametrizado e de copia
     */
    /**
     * Construtor por omissao de Estado.
     */
    public Estado(){
        this.estado = "";
    }

    /**
     * Construtor parametrizado de Estado.
     * Aceita como parametro a String para definir o estado.
     */
    public Estado(String novoEstado){
        this.estado = novoEstado;
    }

    /**
     * Construtor por copia de Estado.
     * Aceita como parametro outro Estado e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Estado(Estado e){
        this.estado = e.getEstado();
    }


    /*
     * Metodos de Instancia
     */
    /**
     * Metodo que devolve o valor do estado.
     * @return valor do estado
     */

    public String getEstado(){
        return this.estado;
    }
    /**
     * Metodo que atualiza o valor do estado.
     * @param novoEstado String do novo estado
     */
    public void setEstado(String novoEstado){
        this.estado = novoEstado;
    }

    /**
     * Metodo que passa o Estado para "Ocupado"
     */
    public void toOcupado(){
        this.estado = "Ocupado";
    }

    /**
     * Metodo que passa o Estado para "Livre"
     */
    public void toLivre(){
        this.estado = "Livre";
    }

    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public Estado clone(){
        return new Estado(this);
    }

    /**
     * Metodo que determina se dois Estados sao iguais.
     * @return boolean verdadeiro caso os valores da String estado dos dois sejam iguais.
     */
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        Estado e = (Estado) o;
        return this.estado.equals(e.getEstado());
    }

    /**
     * Metodo que devolve a representaçao em String do Estado
     * @return String com o estado
     */
    public String toString(){
        return this.estado;
    }
}