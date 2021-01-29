package Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Write a description of class Model.Utilizador here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

/**
 * Classe que guarda toda a informação relativa a um Utilizador
 */

public class Utilizador implements Serializable
{
    private String pass;
    private String cod;
    private String nome;
    private GPS gps;
    private Map<String,List<String>> pedidos;
    private List<Encomenda> entregues;


    /**
     * Construtor sem parâmetros
     */
    public Utilizador()
    {
        this.pass = new String();
        this.cod = new String();
        this.nome = new String();
        this.gps = new GPS();
        this.pedidos = new HashMap<>();
        this.entregues = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param s     String com o codigo de utilizador
     * @param n     String com o nome do utilizador
     * @param gps   GPS com a localização do utilizador
     * @param lista Lista com todas as encomendas feitas
     */
    public Utilizador (String s, String n, GPS gps, ArrayList<Encomenda> lista)
    {
        this.pass = s;
        this.cod = s;
        this.nome = n;
        this.gps = gps.clone();
        this.pedidos = new HashMap<>();
        this.setEntregues(lista);
    }

    /**
     * Construtor parametrizado
     * @param p     String com a pass
     * @param s     String com o codigo de utilizador
     * @param n     String com o nome
     * @param gps   GPS com a localização
     */
    public Utilizador (String p,String s, String n, GPS gps)
    {
        this.pass = p;
        this.cod = s;
        this.nome = n;
        this.gps = gps.clone();
        this.pedidos = new HashMap<>();
        this.entregues = new ArrayList<>();
    }

    /**
     * Construtor por cópia
     * @param u     Utilizador a copiar
     */

    public Utilizador (Utilizador u)
    {
        this.pass = u.getPass();
        this.cod = u.getCod();
        this.nome = u.getNome();
        this.gps = new GPS(u.getGPS());
        this.setPedidos(u.getPedidos());
        this.setEntregues(u.getEntregues());
    }

    /**
     * Set da variavel pass do objeto
     * @param p     String com a pass
     */
    public void setPass(String p){this.pass = p;}

    /**
     * Get da variavel pass do objeto
     * @return      String com a pass
     */
    public String getPass(){return this.pass;}

    /**
     * Get da variavel cod do objeto
     * @return      String com o codigo
     */
    public String getCod()
    {
        return this.cod;
    }

    /**
     * Get da variavel nome do objeto
     * @return      String com o nome
     */
    public String getNome()
    {
        return this.nome;
    }

    /**
     * Get da variavel gps do objeto
     * @return      GPS
     */
    public GPS getGPS()
    {
        return this.gps;
    }

    /**
     * Get da variavel entregues do objeto
     * @return      Lista com todas as encomendas
     */
     public ArrayList<Encomenda> getEntregues() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda l : this.entregues)
            aux.add(l);
        return aux;
    }

    /**
     * Set da variavel entregues do objeto
     * @param l     Lista com todas as encomendas
     */
    public void setEntregues (ArrayList<Encomenda> l)
    {
        this.entregues = new ArrayList<>();
        for(Encomenda li : l)
            this.entregues.add(li);
    }

    /**
     * Get da variavel pedidos do objeto
     * @return      Map com todos os pedidos de transportadoras
     */
    public Map<String,List<String>> getPedidos() {
        Map<String,List<String>> aux = new HashMap<>();
        for (Map.Entry<String,List<String>> l : this.pedidos.entrySet()) {
            if (!aux.containsKey(l.getKey())) aux.put(l.getKey(), new ArrayList<>());
            aux.put(l.getKey(), l.getValue());
        }
        return aux;
    }
    /**
     * Set da variavel pedidos do objeto
     * @param l     Map com todas os pedidos de transportadoras
     */
    public void setPedidos (Map<String,List<String>> l)
    {
        this.pedidos = new HashMap<>();
        for(Map.Entry<String,List<String>> li : l.entrySet()) {
            if (!pedidos.containsKey(li.getKey())) pedidos.put(li.getKey(), new ArrayList<>());
            this.pedidos.put(li.getKey(), li.getValue());
        }
    }

    /**
     * Set da variavel cod do objeto
     * @param s     String com o codigo
     */
    public void setCod(String s)
    {
        this.cod = s;
    }

    /**
     * Set da variavel nome do objeto
     * @param n     String com o nome
     */
    public void setNome(String n)
    {
        this.nome = n;
    }

    /**
     * Set da variavel gps do objeto
     * @param l     double com a latitude
     * @param lo    double com a longitude
     */
    public void setGPS (double l, double lo)
    {
        this.gps.setGPS(l,lo);
    }

    /**
     * Método que clona este objeto
     * @return      Utilizador clone deste objeto
     */
    public Utilizador clone()
    {
        return new Utilizador(this);
    }

    /**
     * Método equals do objeto
     * @param o     Objeto a comparar
     * @return      boolean
     */
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Utilizador u = (Utilizador) o;
        return this.cod.equals(u.getCod()) &&
               this.nome.equals(u.getNome()) &&
               this.gps.equals(u.getGPS());
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Model.Utilizador{");
        sb.append("cod='").append(cod).append('\'');
        sb.append(", nome='").append(nome).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", entregues=").append(entregues);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método que verifica se duas pass são iguais
     * @param p     String com pass
     * @return      boolean
     */
    public boolean validaPass(String p){
        return this.pass.equals(p);
    }

    /**
     * Método que adiciona um elemento a variavel pedidos
     * @param e     String com codigo de encomenda
     * @param t     String com codigo de transportadora
     */
    public void addPedidos(String e, String t){
        if(pedidos.containsKey(e)) pedidos.get(e).add(t);
        else {
            pedidos.put(e,new ArrayList<>());
            pedidos.get(e).add(t);
        }
    }

    /**
     * Método que retira um elemento da variavel pedidos
     * @param e     Encomenda a retirar
     */
    public void rejeitaPedido(Encomenda e){
        this.pedidos.remove(e.getCodenc());
    }

    /**
     * Método que adiciona uma encomenda a variavel entregues
     * @param e     Encomenda a adicionar
     */
    public void addEncomenda(Encomenda e){
        this.entregues.add(e);
    }

    /**
     * Método que procura uma encomenda e faz todas as alterações relativamente a ser aceite para entrega
     * @param e     Encomenda a alterar
     * @param cod   String com codigo de transportadora ou voluntario
     * @param i     LocalDateTime com a data e hora de aceitação
     */
    public void encAceite(Encomenda e,String cod, LocalDateTime i){
        Iterator<Encomenda> it = this.entregues.iterator();
        boolean f = false;
        while(it.hasNext() && !f){
            Encomenda e1 = it.next();
            if(e1.equals(e)){
                e1.setDatai(i);
                e1.setTransp(cod);
                e1.setAceites(true);
                f = true;
                rejeitaPedido(e);
            }
        }
    }

    /**
     * Método que procura ume encomenda e faz todas a alterações relativamente a ser entregue
     * @param e     Encomenda a alterar
     * @param fi    LocalDateTime com a data e hora da entrega
     */
    public void encEntregue(Encomenda e, LocalDateTime fi){
        Iterator<Encomenda> it = this.entregues.iterator();
        boolean f = false;
        while(it.hasNext() && !f){
            Encomenda e1 = it.next();
            if(e1.equals(e)){
                e1.setDataf(fi);
                e1.setEntregue(true);
                f = true;
            }
        }
    }

    /**
     * Set da variavel classificação de uma encomenda
     * @param e     Encomenda a classificar
     * @param cl    int com a classificação
     */

    public void setCl(Encomenda e, int cl){
        Iterator<Encomenda> it = this.entregues.iterator();
        boolean f = false;
        while(it.hasNext() && !f){
            Encomenda e1 = it.next();
            if(e1.equals(e)){
                e1.setClassificacao(cl);
                f = true;
            }
        }
    }

    /**
     * Método que aiciona uma encomenda a lista entregues atraves do parse de um ficheiro
     * @param e     Encomenda a adicionar
     */
    public void addEncomendaParse(Encomenda e) {
        if (!entregues.contains(e)) {
            entregues.add(e);
        }
    }
}