package Projeto.Entidades;
import Projeto.Interfaces.IEmpresa;
import Projeto.Interfaces.IEncomenda;
import Projeto.Interfaces.IEntidade;
import Projeto.Util.Estado;
import Projeto.Util.GPS;

import java.io.Serializable;
import java.util.Collection;
import java.util.Objects;

/**
 * Classe que implementa uma Empresa.
 * Uma empresa é uma extensao da classe abstrata Trabalhador
 */
public class Empresa extends Voluntario implements IEmpresa, Comparable<IEntidade>, Serializable {
    private float taxa;
    private String nif;
    private double dist;

    /*
     * Construtores da Classe Empresa.
     * Declaracao dos construtores por omissao, parametrizado e de copia.
     */
    /**
     * Construtor por omissao de Empresa.
     */
    public Empresa() {
        this.taxa = 1;
        this.nif = "";
        this.dist = 0;
    }
    /**
     * Construtor parametrizado de Empresa.
     * Aceita como parametros duas Strings uma para o id da empresa, outra para o nome da empresa.  Acabar!!!!!!!!!!
     *
     */
    public Empresa(String id, String pw, String nome, GPS loc, Collection<IEncomenda> encs,
                   Collection<Float> vel, float raio, boolean medic,
                   Collection<Integer> cls, Estado e, int cap, float t, String nif, double dist, Collection<IEncomenda> encsPorEntregar) {
        super(id, pw, nome, loc, encs, vel, raio, medic, cls, e, cap, encsPorEntregar);
        this.taxa = t;
        this.nif = nif;
        this.dist = dist;
    }

    /**
     * Construtor por copia de Empresa.
     * Aceita como parametro outra Empresa e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Empresa(Empresa e) {
        super(e);
        this.taxa = e.getTaxa();
        this.nif = e.getNif();
        this.dist = e.getDist();
    }

    /*
     * Getters e Setters
     */
    /**
     * Metodo que devolve a taxa definida pela empresa.
     */
    public float getTaxa() {
        return this.taxa;
    }

    /**
     * Metodo que devolve o nif da empresa.
     */
    public String getNif() {
        return this.nif;
    }


    public double getDist() {
        return this.dist;
    }

    /**
     * Metodo que atualiza a taxa de uma empresa.
     * @param taxa - nova taxa definida pela empresa
     */
    public void setTaxa(float taxa) {
        this.taxa = taxa;
    }

    /**
     * Metodo que atualiza o nif da empresa.
     * @param nif - novo nif
     */
    public void setNif(String nif) {
        this.nif = nif;
    }

    public void setDist(double dist) {
        this.dist = dist;
    }

    /*
     * Restantes Metodos de Instancia
     */
    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public Empresa clone() {
        return new Empresa(this);
    }
    
    /**
     * Metodo que determina se duas Empresas sao iguais.
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Empresa e = (Empresa) o;
        return super.equals(e);
    }
    
    /**
     * Metodo que devolve a representaçao em String da Empresa.
     * @return String com as variaveis desta instancia.
     */
    public String toString() {
        return super.toString() +
               "\nTaxa: " + this.taxa +
                "\nNif: " + this.nif;
    }

    /**
     * Método hashCode
     */
    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), taxa, nif, dist);
    }
}