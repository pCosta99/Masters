package Projeto.Entidades;

import Projeto.Interfaces.IEncomenda;
import Projeto.Interfaces.IVoluntario;
import Projeto.Util.Estado;
import Projeto.Util.GPS;

import java.io.Serializable;
import java.util.Collection;
import java.util.ArrayList;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Classe abstrata que implementa um trabalhador.
 * Esta pode derivar num voluntario ou numa empresa, sendo a classe Trabalhador util para manipular os dados que estas duas classes tem em comum.
 * A Trabalhador contem uma lista de velocidades para cada encomenda que entregam,
 * um raio para delimitar a zona que um trabalhador presta um serviço de entrega,
 * um booleano para verificar se aceita transportar equipamentos medicos,
 * uma lista de classificacoes submetidas pelos utilizadores da aplicacao para cada entrega,
 * um estado com o objetivo de considerar se o trabalhador se encontra disponivel
 * e ainda um int para a capacidade maxima de produtos que um trabalhador, seja ele um voluntario ou uma empresa, consegue transportar.
 */

public class Voluntario extends Entidade implements Serializable, IVoluntario {
    private Collection<Float> vel;
    private float raio;
    private boolean medic;
    private Collection<Integer> classificacao;
    private Estado estado;
    private int cap_max;
    private Collection<IEncomenda> encsPorEntregar;


    /**
     * Construtor por omissao do Trabalhador.
     */

    public Voluntario() {
        super();
        this.vel = new ArrayList<>();
        this.raio = 0.0f;
        this.medic = false;
        this.classificacao = new ArrayList<>();
        this.estado = new Estado();
        this.cap_max = 0;
        this.encsPorEntregar = new ArrayList<>();
    }

    /**
     * Construtor parametrizado do Trabalhador.
     */

    public Voluntario(String id, String pw, String nome, GPS loc, Collection<IEncomenda> encs, Collection<Float> vel,
                      float raio, boolean medic, Collection<Integer> cls, Estado e, int cap_max, Collection<IEncomenda> encsP) {
        super(id, pw, nome, loc, encs);
        setVel(vel);
        setRaio(raio);
        aceitaMedicamentos(medic);
        setClassificacao(cls);
        setEstado(e);
        setCapMax(cap_max);
        setEncomendasPorEntregar(encsP);
    }

    /**
     * Construtor por copia do Trabalhador.
     * Aceita como parametro outro Trabalhador e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Voluntario(Voluntario t) {
        super(t);
        this.vel = t.getVel();
        this.raio = t.getRaio();
        this.medic = t.aceitoTransporteMedicamentos();
        this.classificacao = t.getClassificacao();
        this.estado = t.getEstado();
        this.cap_max = t.getCapMax();
        this.encsPorEntregar = t.getEncomendasPorEntregar();
    }

    /*
     * Getters e Setters
     */

    /**
     * Metodo que retorna a lista de velocidades.
     */
    protected Collection<Float> getVel() {
        return new ArrayList<>(this.vel);
    }

    /**
     * Metodo que retorna o raio de açao em que o trabalhador realiza as entregas.
     */
    public float getRaio() {
        return this.raio;
    }

    /**
     * Metodo que retorna um booleano verdadeiro se o trabalhador aceitar transportar produtos medicinais.
     * Apenas deve ser chamado caso o Trabalhador tenha certificado de transporte de medicamentos.
     */
    public boolean aceitoTransporteMedicamentos() {
        return this.medic;
    }

    /**
     * Metodo que retorna a lista de classificacoes.
     */
    public Collection<Integer> getClassificacao() {
        return new ArrayList<>(this.classificacao);
    }

    /**
     * Metodo que retorna o estado (livre ou ocupado) do trabalhador.
     */
    public Estado getEstado() { //mudei para public
        return this.estado;
    }

    /**
     * Metodo que retorna a capacidade máxima de produtos a serem transportados por um trabalhador.
     */
    public int getCapMax() {
        return this.cap_max;
    }

    public Collection<IEncomenda> getEncomendasPorEntregar() {
        return this.encsPorEntregar.stream().map(IEncomenda::clone).collect(Collectors.toList());
    }

    /**
     * Metodo que altera a lista de velocidades de um trabalhador.
     *
     * @param lf - nova velocidade
     */
    public void setVel(Collection<Float> lf) {
        this.vel = new ArrayList<>(lf);
    }

    /**
     * Metodo que altera o raio de açao de um trabalhadodr.
     *
     * @param raio - novo raio de açao
     */
    public void setRaio(float raio) {
        this.raio = raio;
    }

    /**
     * Metodo que altera a decisão de poder transportar produtos medicinais.
     */
    public void aceitaMedicamentos(boolean medic) {
        this.medic = medic;
    }

    /**
     * Metodo que altera a lista de classificacoes de um trabalhador.
     *
     * @param l - nova classificaçao?????????????
     */
    public void setClassificacao(Collection<Integer> l) {
        this.classificacao = new ArrayList<>(l);
    }

    /**
     * Metodo que altera o estado de um trabalhador.
     *
     * @param e - novo Estado
     */
    public void setEstado(Estado e) {
        this.estado = e;
    }

    /**
     * Metodo que altera a capacidade máxima de produtos que podem ser transportados por um trabalhador.
     *
     * @param cap_max - nova capacidade maxima
     */
    public void setCapMax(int cap_max) {
        this.cap_max = cap_max;
    }

    public void setEncomendasPorEntregar(Collection<IEncomenda> encs) {
        this.encsPorEntregar = encs.stream().map(IEncomenda::clone).collect(Collectors.toList());
    }

    /*
     * Restantes Metodos de Instancia
     */

    public void addEncomendaPorEntregar(IEncomenda e) {
        this.encsPorEntregar.add(e.clone());
    }

    public void switchEncomenda(IEncomenda e) {
        this.encsPorEntregar.remove(e);
        this.adicionaEnc(e);
    }

    /**
     * Metodo que insere uma classificacao na lista.
     *
     * @param cl - nova classificaçao
     */
    public void insereClassificacao(int cl) {
        if (this.classificacao != null) {
            this.classificacao.add(cl);
        }
    }

    /**
     * Metodo que verifica se um trabalhador tem capacidade para transportar a encomenda em questao
     */
    public boolean temCapacidade(boolean med) {
        boolean ret = true;
        if(med && !this.medic) ret = false;
        return ret && this.getEncomendasPorEntregar().size() <= this.getCapMax();
    }

    /**
     * Metodo que insere uma velocidade na lista.
     *
     * @param v - nova velocidade
     */
    public void insereVel(float v) {
        if (this.vel != null) {
            this.vel.add(v);
        }
    }

    /**
     * Metodo que calcula a velocidade media de um trabalhador, com base nas velocidades registadas.
     */
    public float calculaVelMed() {
        float soma = 0;
        int size = 1;
        for (float v : this.vel) {
            soma += v;
        }
        if (this.vel.size() > 0) size = this.vel.size();
        return soma / size;
    }

    /**
     * Metodo que calcula a media das classificacoes de um trabalhador.
     */
    protected float calculaClassMed() {
        float soma = 0;
        for (int c : this.classificacao) {
            soma += c;
        }
        return soma / this.classificacao.size();
    }

    /**
     * Método que permite adicionar uma Encomenda à lista de encomendas, tendo em conta a capacidade máxima.
     */
    public boolean canAddEncomenda() {
        return this.getEncomendas().size() + 1 < this.cap_max;
    }

    /**
     * Metodo que determina se dois Trabalhadores sao iguais.
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Voluntario t = (Voluntario) o;
        return super.equals(t);
    }

    /**
     * Metodo que devolve a representaçao em String do Trabalhador.
     *
     * @return String com as variaveis desta instancia.
     */
    public String toString() {
        return super.toString() +
                "\nVelocidade: " + this.vel +
                "\nRaio: " + this.raio +
                "\nAceita medicamentos? " + this.medic +
                "\nClassificaçao" + this.classificacao +
                "\nEstado: " + this.estado +
                "\nCapacidade máxima: " + this.cap_max;

    }

    public Voluntario clone() {
        return new Voluntario(this);
    }

    /**
     * Método hashCode
     */
    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), vel, raio, medic, classificacao, estado, cap_max, encsPorEntregar);
    }
}