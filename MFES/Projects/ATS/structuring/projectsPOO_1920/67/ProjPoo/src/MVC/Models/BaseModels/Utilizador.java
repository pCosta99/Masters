package MVC.Models.BaseModels;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Class para um User do tipo Utilizador e onde as funcionalidades
 * que este poderá usar.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */
public class Utilizador extends User implements Serializable {
    private List<String> porAceitar;
    private List<String> porClassificar;
    private int numeroEncomendas;

    // aceita encomenda
    /**
     * Construtor de Utilizador por defeito.
     */
    public Utilizador() {
        super();
        this.porAceitar = new ArrayList<>();
        this.porClassificar = new ArrayList<>();
        this.numeroEncomendas = 0;
    }

    /**
     * Construtor de Utilizador parametrizado.
     * @param c Código do Utilizador.
     * @param n Nome do Utilizador.
     * @param x Coordenada X do Utilizador.
     * @param y Coordenada Y do Utilizador.
     */
    public Utilizador(String c, String n, double x, double y) {
        super(c, n, x, y);
        this.porAceitar = new ArrayList<>();
        this.porClassificar = new ArrayList<>();
        this.numeroEncomendas = 0;
    }

    /**
     * Construtor de Utilizador por Cópia.
     * @param user Utilizador a copiar.
     */
    public Utilizador(Utilizador user) {
        super(user);
        this.porAceitar = user.getPorAceitar();
        this.porClassificar = user.getPorClassificar();
        this.numeroEncomendas = user.getNumeroEncomendas();
    }

    /**
     * Método que retorna uma Cópia da Lista de Códigos de encomendas que ainda não aceitou.
     * @return Cópia da Lista.
     */
    public List<String> getPorAceitar() {
        return new ArrayList<>(this.porAceitar);
    }

    /**
     * Método que adiciona um Código de Encomenda à Lista porAceitar.
     * @param key Código da Encomenda.
     */
    public void addKeyPorAceitar(String key) {
        this.porAceitar.add(key);
    }

    /**
     * Método que retorna uma Cópia da Lista de Códigos de encomendas que ainda não classificou.
     * @return
     */
    public List<String> getPorClassificar() {
        return new ArrayList<>(this.porClassificar);
    }

    /**
     * Método que adiciona um Código de Encomenda à Lista porClassificar.
     * @param key Código da Encomenda.
     */
    public void addKeyPorClassificar(String key) {
        this.porClassificar.add(key);
    }

    /**
     * Método toString.
     * @return String com os dados de Utilizador.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador{ \n").append(super.toString()).append("\nNumero de encomendas: ").append(this.numeroEncomendas).append("\n}");
        return sb.toString();
    }

    /**
     * Método que cria uma Encomenda.
     * @param codEncomenda Código da Encomenda.
     * @param loja Código da Loja.
     * @param peso Peso do Produto.
     * @param le Contéudo da Encomenda.
     * @return
     */
    public Encomenda criaEncomenda(String codEncomenda,String loja,double peso,List<LinhaEncomenda> le){
        this.addKeyPorAceitar(codEncomenda);
        return new Encomenda(codEncomenda,this.getCod(),loja,peso,le);
    }

    /**
     * Método em que o Utilizador aceita uma Encomenda.
     * Caso Aceita a encomenda, é adicionada à Lista de encomendas por Classificar.
     * @param cod Código de uma Encomenda.
     * @param b True caso aceite a encomenda, false caso contrário.
     */
    public void aceitaEncomenda(String cod,boolean b){
            this.porAceitar.remove(cod);
        if(b)
            this.numeroEncomendas++;
            this.addKeyPorClassificar(cod);
    }

    /**
     * Método que após uma encomenda ser classificada,
     * é adicionada à Lista de encomendas entregues ao Utilizador.
     * @param cod Código da Encomenda.
     */
    public void classificaEncomenda(String cod){
        this.porClassificar.remove(cod);
        this.addEncomenda(cod);
    }

    /**
     * Método que devolve o Número de encomendas realizadas pelo Utilizador.
     * @return Número de encomendas.
     */
    public int getNumeroEncomendas(){
        return this.numeroEncomendas;
    }

    /**
     * Método que define o Número de encomendas realizadas pelo Utilizador.
     * @param numE Número de encomendas.
     */
    public void setNumeroEncomendas(int numE){
        this.numeroEncomendas = numE;
    }

    /**
     * Método Clone.
     * @return Utilizador Clonado.
     */
    public Utilizador clone(){
        return new Utilizador(this);
    }
}